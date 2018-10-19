package inc.typechecker

import cats.data.Chain
import cats.syntax.functor._
import inc.common._
import java.lang.String
import scala.{ Either, Right, None, StringContext }
import scala.collection.immutable.{ List, Map }
import scala.Predef.{ ArrowAssoc, augmentString }

object Typechecker {
  type Environment = Map[String, TypeScheme]
  type Substitution = Map[TypeVariable, Type]

  val EmptyEnv: Environment = Map.empty
  val EmptySubst: Substitution = Map.empty
  val EmptyResult: Either[List[TypeError], (Chain[Expr[NamePosType]], Substitution)] = Right((Chain.empty, EmptySubst))

  def trace(name: String, pos: Pos, typ: Type, source: String) = {
    val formattedMsg = name + ": " + Printer.print(typ)
    scribe.info(Printer.withSourceContext(None, formattedMsg, pos, fansi.Color.Yellow, source))
  }

  def trace(name: String, pos: Pos, typ: TypeScheme, source: String) = {
    val formattedMsg = name + ": " + Printer.print(typ)
    scribe.info(Printer.withSourceContext(None, formattedMsg, pos, fansi.Color.Yellow, source))
  }

  def trace(name: String, env: Environment) = {
    val formattedMsg = NL + name + ": " + (NL * 2) + env.map { case (nm, tp) => nm + ": " + Printer.print(tp) }.mkString(NL)
    scribe.info(formattedMsg)
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
    if (subst.nonEmpty) scribe.info(NL + "Apply substitution: " + Printer.print(subst))
    env.mapValues(_.substitute(subst))
  }

  def chainSubstitution(s1: Substitution, s2: Substitution): Substitution =
    s2 ++ s1.mapValues(_.substitute(s2))

  def unify(pos: Pos, left: Type, right: Type): Either[List[TypeError], Substitution] = {
    val ll = Printer.print(left)
    val rr = Printer.print(right)

    val llYellow = Yellow(ll)
    val rrYellow = Yellow(rr)

    scribe.info(NL + s"Unify $llYellow with $rrYellow")

    val llRed = Red(ll)
    val rrRed = Red(rr)

    (left, right) match {
      case (TypeConstructor(_, lvars), TypeConstructor(_, rvars)) if lvars.length != rvars.length =>
        TypeError.singleton(pos, s"Cannot unify $llRed with $rrRed")
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
        TypeError.singleton(pos, s"$llRed does not unify with $rrRed")
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
        r1 <- typecheck(cond, env, source)
        (c, s1) = r1

        NamePosType(_, _, condType) = c.meta

        _ = trace("If condition", c.meta.pos, condType, source)

        // Typecheck the then expression
        r2 <- typecheck(thenExpr, env, source)
        (t, s2) = r2

        NamePosType(_, _, thenType) = t.meta

        _ = trace("Then expression", t.meta.pos, thenType, source)

        // Typecheck the else expression
        r3 <- typecheck(elseExpr, env, source)
        (e, s3) = r3

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

        _ = if (s.nonEmpty) scribe.info(NL + "Apply substitution: " + Printer.print(s))

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
        r <- typecheck(body, env ++ paramMappings, source)
        (b, s1) = r

        _ = trace("Lambda body", b.meta.pos, b.meta.typ, source)

        (bTp, s2) = b.meta.typ.instantiate

        // Create a new function type
        tp = Type.Function(typedParams.map(_.meta.typ.typ), bTp)

        s = chainSubstitution(s1, s2)

        _ = if (s.nonEmpty) scribe.info(NL + "Apply substitution: " + Printer.print(s))

        // Apply the substitutions from the body
        expr = Lambda(typedParams, b, meta.withSimpleType(tp)).substitute(s)

        _ = trace("Lambda expression", expr.meta.pos, expr.meta.typ, source)

      } yield (expr, s)

    case Apply(fn, args, meta) =>
      val tv = TypeVariable()

      trace("Function application", meta.pos, tv, source)

      for {
        // Typecheck the function
        rf <- typecheck(fn, env, source)
        (f, s1) = rf

        _ = trace("Function to apply", f.meta.pos, f.meta.typ, source)

        // Typecheck the arg expressions
        ra <- args.zipWithIndex.foldLeft(EmptyResult) {
          case (resSoFar, (nextArg, idx)) =>
            for {
              r <- resSoFar
              (typedSoFar, substSoFar) = r

              a <- typecheck(nextArg, substitute(env, s1), source)
              (typedArg, newSubst) = a

              _ = trace(s"Argument ${idx + 1}", typedArg.meta.pos, typedArg.meta.typ, source)

            } yield (typedSoFar :+ typedArg, chainSubstitution(substSoFar, newSubst))
        }

        (as, s2) = ra

        s3 = chainSubstitution(s1, s2)

        argsList = as.toList

        (argTps, s4) = argsList.foldLeft((Chain.empty[Type], s3)) {
          case ((tps, subst), arg) =>
            val (argTp, argSubst) = arg.meta.typ.instantiate
            (tps :+ argTp, chainSubstitution(subst, argSubst))
        }

        // Create a new function type
        appliedTp = Type.Function(argTps.toList, tv).substitute(s4)

        _ = trace("Applied type", meta.pos, appliedTp, source)

        (fnTp, s5) = f.meta.typ.instantiate

        // Unify the function type with the actual argument types
        s6 <- unify(meta.pos, fnTp, appliedTp)

        s = chainSubstitutions(s4, s5, s6)

        _ = if (s.nonEmpty) scribe.info(NL + "Apply substitution: " + Printer.print(s))

        expr = Apply(f, argsList, meta.withType(TypeScheme(tv))).substitute(s)

      } yield (expr, s)
  }

  def typecheck(decl: TopLevelDeclaration[NameWithPos], env: Environment, source: String): Either[List[TypeError], (TopLevelDeclaration[NamePosType], Environment)] = decl match {
    case Let(name, expr, meta) =>
      typecheck(expr, env, source).flatMap {
        case (checkedExpr, subst) =>
          val (eTp, _) = checkedExpr.meta.typ.instantiate
          val tp = TypeScheme.generalize(env, eTp)
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
            r <- resSoFar
            (checkedSoFar, envSoFar) = r
            c <- typecheck(nextDecl, envSoFar, source)
            (checkedDecl, updatedEnv) = c
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
