package inc.typechecker

import cats.data.Chain
import cats.syntax.either._
import cats.syntax.functor._
import inc.common._
import java.lang.String
import scala.{ Boolean, Either, Right, None, StringContext }
import scala.collection.immutable.{ List, Map }
import scala.Predef.{ ArrowAssoc, augmentString }

object Typechecker extends Typechecker(true)

class Typechecker(isTraceEnabled: Boolean) {
  type Environment = Map[String, TypeScheme]
  type Substitution = Map[TypeVariable, Type]

  val EmptyEnv: Environment = Map.empty
  val EmptySubst: Substitution = Map.empty
  val EmptyResult: Either[List[TypeError], (Chain[Expr[NamePosType]], Substitution)] = Right((Chain.empty, EmptySubst))

  def trace(name: String, pos: Pos, typ: Type, source: String) = {
    val formattedMsg = name + ": " + Printer.print(typ)
    if (isTraceEnabled) scribe.info(Printer.withSourceContext(None, formattedMsg, pos, fansi.Color.Yellow, source))
  }

  def trace(name: String, pos: Pos, typ: TypeScheme, source: String) = {
    val formattedMsg = name + ": " + Printer.print(typ)
    if (isTraceEnabled) scribe.info(Printer.withSourceContext(None, formattedMsg, pos, fansi.Color.Yellow, source))
  }

  def trace(name: String, env: Environment) = {
    val formattedMsg = NL + name + ": " + (NL * 2) + env.map { case (nm, tp) => nm + ": " + Printer.print(tp) }.mkString(NL)
    if (isTraceEnabled) scribe.info(formattedMsg)
  }

  def bind(pos: Pos, tyVar: TypeVariable, typ: Type): Either[List[TypeError], Substitution] = typ match {
    case t @ TypeVariable(_) if tyVar == t =>
      Right(EmptySubst)
    case t if tyVar.occursIn(t) =>
      TypeError.singleton(pos, "Attempt to construct infinite type")
    case _ =>
      Right(Map(tyVar -> typ))
  }

  def chainSubstitutions(ss: List[Substitution]): Substitution =
    ss.foldLeft(Map.empty: Substitution)(chainSubstitution)

  def chainSubstitutions(ss: Substitution*): Substitution =
    chainSubstitutions(ss.toList)

  def substitute(env: Environment, subst: Substitution): Environment = {
    if (subst.nonEmpty && isTraceEnabled) scribe.info(NL + "Apply substitution: " + Printer.print(subst))
    env.mapValues(_.substitute(subst))
  }

  def chainSubstitution(s1: Substitution, s2: Substitution): Substitution =
    s2 ++ s1.mapValues(_.substitute(s2))

  def unify(pos: Pos, left: Type, right: Type): Either[List[TypeError], Substitution] = {
    lazy val ll = Printer.print(left)
    lazy val rr = Printer.print(right)
    lazy val llRed = Red(ll)
    lazy val rrRed = Red(rr)

    if (isTraceEnabled) {
      val llYellow = Yellow(ll)
      val rrYellow = Yellow(rr)
      scribe.info(NL + s"Unify $llYellow with $rrYellow")
    }

    def go(left: Type, right: Type): Either[List[TypeError], Substitution] = {
      (left, right) match {
        case (TypeConstructor(_, lvars), TypeConstructor(_, rvars)) if lvars.length != rvars.length =>
          TypeError.singleton(pos, s"${llRed} does not unify with ${rrRed}")
        case (TypeConstructor(l, lvars), TypeConstructor(r, rvars)) if l == r =>
          val emptyRes: Either[List[TypeError], Substitution] = Right(EmptySubst)

          lvars.zip(rvars).foldLeft(emptyRes) {
            case (substSoFar, (ll, rr)) =>
              for {
                subst <- substSoFar
                newSubst <- unify(pos, ll.substitute(subst), rr.substitute(subst))
              } yield chainSubstitution(subst, newSubst)
          }

        case (tyVar @ TypeVariable(_), typ) =>
          bind(pos, tyVar, typ)

        case (typ, tyVar @ TypeVariable(_)) =>
          bind(pos, tyVar, typ)

        case (_, _) =>
          TypeError.singleton(pos, s"${llRed} does not unify with ${rrRed}")
      }
    }

    go(left, right).leftFlatMap { _ =>
      TypeError.singleton(pos, s"${llRed} does not unify with ${rrRed}")
    }
  }

  def withSimpleType(expr: Expr[NameWithPos], typ: Type): Either[List[TypeError], (Expr[NamePosType], Substitution)] =
    withTypeScheme(expr, TypeScheme(typ))

  def withTypeScheme(expr: Expr[NameWithPos], typ: TypeScheme): Either[List[TypeError], (Expr[NamePosType], Substitution)] = {
    val exprWithType = expr.map(_.withType(typ))
    Right((exprWithType, EmptySubst))
  }

  def typecheck(expr: Expr[NameWithPos], env: Environment, source: String): Either[List[TypeError], (Expr[NamePosType], Substitution)] = expr match {
    case int @ LiteralInt(_, _) =>
      withSimpleType(int, Type.Int)

    case long @ LiteralLong(_, _) =>
      withSimpleType(long, Type.Long)

    case float @ LiteralFloat(_, _) =>
      withSimpleType(float, Type.Float)

    case double @ LiteralDouble(_, _) =>
      withSimpleType(double, Type.Double)

    case bool @ LiteralBoolean(_, _) =>
      withSimpleType(bool, Type.Boolean)

    case char @ LiteralChar(_, _) =>
      withSimpleType(char, Type.Char)

    case str @ LiteralString(_, _) =>
      withSimpleType(str, Type.String)

    case unit @ LiteralUnit(_) =>
      withSimpleType(unit, Type.Unit)

    case ref @ Reference(name, meta)  =>
      env.get(name).map { typ =>
        trace(s"Reference to $name", meta.pos, typ, source)
        withTypeScheme(ref, typ)
      }.getOrElse(TypeError.singleton(meta.pos, s"Reference to undefined symbol: $name"))

    case If(cond, thenExpr, elseExpr, meta) =>
      for {
        // Typecheck the condition
        (c, s1) <- typecheck(cond, env, source)

        NamePosType(_, _, condType) = c.meta

        _ = trace("If condition", c.meta.pos, condType, source)

        // Typecheck the then expression
        (t, s2) <- typecheck(thenExpr, env, source)

        NamePosType(_, _, thenType) = t.meta

        _ = trace("Then expression", t.meta.pos, thenType, source)

        // Typecheck the else expression
        (e, s3) <- typecheck(elseExpr, env, source)

        NamePosType(_, _, elseType) = e.meta

        _ = trace("Else expression", e.meta.pos, elseType, source)

        (cTp, s4) = condType.instantiate

        // Unify the condition with Boolean
        s5 <- unify(c.meta.pos, cTp, Type.Boolean)

        (tTp, s6) = thenType.instantiate

        (eTp, s7) = elseType.instantiate

        // Unify the then expression and the else expression
        s8 <- unify(meta.pos, tTp, eTp)

        s = chainSubstitutions(s1, s2, s3, s4, s5, s6, s7, s8)

        _ = if (s.nonEmpty && isTraceEnabled) scribe.info(NL + "Apply substitution: " + Printer.print(s))

        expr = If(c, t, e, meta.withType(thenType)).substitute(s)

        _ = trace("If expression", expr.meta.pos, expr.meta.typ, source)

      } yield (expr, s)

    case Lambda(params, body, meta) =>
      val typedParams = params.map {
        case Param(name, meta) =>
          Param(name, meta.withSimpleType(TypeVariable()))
      }

      val paramMappings = typedParams.map(p => p.name -> p.meta.typ)

      typedParams.foreach {
        case Param(name, meta) =>
          trace(name, meta.pos, meta.typ, source)
      }

      for {
        // Typecheck the body with the params in scope
        (b, s1) <- typecheck(body, env ++ paramMappings, source)

        _ = trace("Lambda body", b.meta.pos, b.meta.typ, source)

        (bTp, s2) = b.meta.typ.instantiate

        // Create a new function type
        tp = Type.Function(typedParams.map(_.meta.typ.typ), bTp)

        s = chainSubstitution(s1, s2)

        _ = if (isTraceEnabled && s.nonEmpty) scribe.info(NL + "Apply substitution: " + Printer.print(s))

        // Apply the substitutions from the body
        expr = Lambda(typedParams, b, meta.withSimpleType(tp)).substitute(s)

        _ = trace("Lambda expression", expr.meta.pos, expr.meta.typ, source)

      } yield (expr, s)

    case Apply(fn, args, meta) =>
      val tv = TypeVariable()

      trace("Function application", meta.pos, tv, source)

      for {
        // Typecheck the function
        (f, s1) <- typecheck(fn, env, source)

        _ = trace("Function to apply", f.meta.pos, f.meta.typ, source)

        initialResult = Either.right[List[TypeError], (Chain[Expr[NamePosType]], Substitution)]((Chain.empty, s1))

        // Typecheck the arg expressions
        (as, s2) <- args.zipWithIndex.foldLeft(initialResult) {
          case (resSoFar, (nextArg, idx)) =>
            for {
              (typedSoFar, substSoFar) <- resSoFar

              (typedArg, newSubst) <- typecheck(nextArg, substitute(env, substSoFar), source)

              _ = trace(s"Argument ${idx + 1}", typedArg.meta.pos, typedArg.meta.typ, source)

            } yield (typedSoFar :+ typedArg, chainSubstitution(substSoFar, newSubst))
        }

        argsList = as.toList

        (argTps, s3) = argsList.foldLeft((Chain.empty[Type], s2)) {
          case ((tps, subst), arg) =>
            val (argTp, argSubst) = arg.meta.typ.instantiate
            (tps :+ argTp, chainSubstitution(subst, argSubst))
        }

        // Create a new function type
        appliedTp = Type.Function(argTps.toList, tv).substitute(s3)

        _ = trace("Applied type", meta.pos, appliedTp, source)

        (fnTp, s4) = f.meta.typ.instantiate

        s5 = chainSubstitution(s3, s4)

        // Unify the function type with the actual argument types
        s6 <- unify(meta.pos, fnTp.substitute(s5), appliedTp)

        s = chainSubstitutions(s3, s4, s5, s6)

        _ = if (isTraceEnabled && s.nonEmpty) scribe.info(NL + "Apply substitution: " + Printer.print(s))

        expr = Apply(f, argsList, meta.withType(TypeScheme(tv))).substitute(s)

      } yield (expr, s)
  }

  def typecheck(decl: TopLevelDeclaration[NameWithPos], env: Environment, source: String): Either[List[TypeError], (TopLevelDeclaration[NamePosType], Environment)] = decl match {
    case Let(name, expr, meta) =>
      typecheck(expr, env, source).flatMap {
        case (checkedExpr, subst) =>
          val (eTp, s) = checkedExpr.meta.typ.instantiate
          if (isTraceEnabled) scribe.info(NL + "Instantiate: " + Printer.print(s))
          val tp = TypeScheme.generalize(env, eTp)
          if (isTraceEnabled && tp.bound.nonEmpty) scribe.info(NL + "Generalize: " + tp.bound.map(Printer.print(_)).mkString("[", ", ", "]"))
          trace(name, meta.pos, tp, source)
          Right((Let(name, checkedExpr, meta.withType(tp)), substitute(env.updated(name, tp), subst)))
      }
  }

  def typecheck(
    module: Module[NameWithPos],
    importedDecls: Map[String, TopLevelDeclaration[NameWithType]],
    source: String
  ): Either[List[TypeError], Module[NamePosType]] = module match {
    case Module(_, _, _, decls, meta) =>

      val initialEnv = importedDecls.mapValues(_.meta.typ)

      val emptyRes: Either[List[TypeError], (Chain[TopLevelDeclaration[NamePosType]], Environment)] =
        Right((Chain.empty, initialEnv))

      val typecheckedDecls = decls.foldLeft(emptyRes) {
        case (resSoFar, nextDecl) =>
          for {
            (checkedSoFar, envSoFar) <- resSoFar
            (checkedDecl, updatedEnv) <- typecheck(nextDecl, envSoFar, source)
          } yield (checkedSoFar :+ checkedDecl, updatedEnv)
      }

      typecheckedDecls.map {
        case (checked, env) =>

          trace("Final type environment", env)

          module.copy(
            declarations = checked.toList,
            meta = meta.withSimpleType(Type.Module)
          )
      }
  }
}
