package inc.typechecker

import cats.data.Chain
import cats.syntax.functor._
import inc.common._

object Typechecker {
  type Environment = Map[String, TypeScheme]
  type Substitution = Map[TypeVariable, Type]

  val EmptyEnv: Environment = Map.empty
  val EmptySubst: Substitution = Map.empty
  val EmptyResult: Either[List[TypeError], (Chain[Expr[NamePosType]], Substitution)] = Right((Chain.empty, EmptySubst))

  def trace(prog: String, name: String, pos: Pos, typ: Type) = {
    val formattedMsg = name + ": " + Printer.print(typ)
    scribe.info(Printer.withSourceContext(None, formattedMsg, pos, fansi.Color.Yellow, prog))
  }

  def trace(prog: String, name: String, pos: Pos, typ: TypeScheme) = {
    val formattedMsg = name + ": " + Printer.print(typ)
    scribe.info(Printer.withSourceContext(None, formattedMsg, pos, fansi.Color.Yellow, prog))
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

  def typecheck(prog: String, expr: Expr[NameWithPos], env: Environment): Either[List[TypeError], (Expr[NamePosType], Substitution)] = expr match {
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
        trace(prog, s"Reference to $name", meta.pos, typ)
        withTypeScheme(ref, typ)
      }.getOrElse(TypeError.singleton(meta.pos, s"Reference to undefined symbol: $name"))

    case If(cond, thenExpr, elseExpr, meta) =>
      for {
        // Typecheck the condition
        r1 <- typecheck(prog, cond, env)
        (c, s1) = r1

        NamePosType(_, _, condType) = c.meta

        _ = trace(prog, "If condition", c.meta.pos, condType)

        // Typecheck the then expression
        r2 <- typecheck(prog, thenExpr, env)
        (t, s2) = r2

        NamePosType(_, _, thenType) = t.meta

        _ = trace(prog, "Then expression", t.meta.pos, thenType)

        // Typecheck the else expression
        r3 <- typecheck(prog, elseExpr, env)
        (e, s3) = r3

        NamePosType(_, _, elseType) = e.meta

        _ = trace(prog, "Else expression", e.meta.pos, elseType)

        // Unify the condition with Boolean
        s4 <- unify(c.meta.pos, condType.instantiate, Type.Boolean)

        // Unify the then expression and the else expression
        s5 <- unify(meta.pos, thenType.instantiate, elseType.instantiate)

        s = chainSubstitutions(s1, s2, s3, s4, s5)

        _ = if (s.nonEmpty) scribe.info(NL + "Apply substitution: " + Printer.print(s))

        expr = If(c, t, e, meta.withType(thenType)).substitute(s)

        _ = trace(prog, "If expression", expr.meta.pos, expr.meta.typ)

      } yield (expr, s)

    case Lambda(params, body, meta) =>
      val typedParams = params.map {
        case Param(name, meta) =>
          Param(name, meta.withSimpleType(TypeVariable()))
      }

      val paramMappings = typedParams.map(p => p.name -> p.meta.typ)

      typedParams.foreach {
        case Param(name, meta) =>
          trace(prog, name, meta.pos, meta.typ)
      }

      for {
        // Typecheck the body with the params in scope
        r <- typecheck(prog, body, env ++ paramMappings)
        (b, s) = r

        _ = trace(prog, "Lambda body", b.meta.pos, b.meta.typ)

        // Create a new function type
        tp = Type.Function(typedParams.map(_.meta.typ.typ), b.meta.typ.instantiate)

        _ = if (s.nonEmpty) scribe.info(NL + "Apply substitution: " + Printer.print(s))

        // Apply the substitutions from the body
        ts = TypeScheme.generalize(env, tp.substitute(s))

        expr = Lambda(typedParams.map(_.substitute(s)), b.substitute(s), meta.withType(ts))

        _ = trace(prog, "Lambda expression", expr.meta.pos, expr.meta.typ)

      } yield (expr, s)

    case Apply(fn, args, meta) =>
      val tv = TypeVariable()

      trace(prog, "Function application", meta.pos, tv)

      for {
        // Typecheck the function
        rf <- typecheck(prog, fn, env)
        (f, s1) = rf

        _ = trace(prog, "Function to apply", f.meta.pos, f.meta.typ)

        // Typecheck the arg expressions
        ra <- args.zipWithIndex.foldLeft(EmptyResult) {
          case (resSoFar, (nextArg, idx)) =>
            for {
              r <- resSoFar
              (typedSoFar, substSoFar) = r

              a <- typecheck(prog, nextArg, substitute(env, s1))
              (typedArg, newSubst) = a

              _ = trace(prog, s"Argument ${idx + 1}", typedArg.meta.pos, typedArg.meta.typ)

            } yield (typedSoFar :+ typedArg, chainSubstitution(substSoFar, newSubst))
        }

        (as, s2) = ra

        argsList = as.toList

        // Create a new function type
        tp = Type.Function(argsList.map(_.meta.typ.instantiate), tv).substitute(s2)

        _ = trace(prog, "Expected type", meta.pos, tp)

        // Unify the function type with the actual argument types
        s3 <- unify(meta.pos, f.meta.typ.instantiate, tp)

        s = chainSubstitutions(s1, s2, s3)

        _ = if (s.nonEmpty) scribe.info(NL + "Apply substitution: " + Printer.print(s))

        expr = Apply(f.substitute(s), argsList.map(_.substitute(s)), meta.withType(TypeScheme(tv.substitute(s))))

      } yield (expr, s2)
  }

  def typecheck(prog: String, decl: TopLevelDeclaration[NameWithPos], env: Environment): Either[List[TypeError], (TopLevelDeclaration[NamePosType], Environment)] = decl match {
    case Let(name, expr, meta) =>
      typecheck(prog, expr, env).flatMap {
        case (checkedExpr, subst) =>
          val tp = checkedExpr.meta.typ
          trace(prog, name, meta.pos, tp)
          Right((Let(name, checkedExpr, meta.withType(tp)), substitute(env, subst).updated(name, checkedExpr.meta.typ)))
      }
  }

  def typecheck(
    prog: String,
    module: Module[NameWithPos],
    importedDecls: Map[String, TopLevelDeclaration[NameWithType]] = Map.empty
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
            c <- typecheck(prog, nextDecl, envSoFar)
            (checkedDecl, updatedEnv) = c
          } yield (checkedSoFar :+ checkedDecl, updatedEnv)
      }

      typecheckedDecls.map {
        case (checked, _) =>
          module.copy(
            declarations = checked.toList,
            meta = meta.withSimpleType(Type.Module)
          )
      }
  }
}
