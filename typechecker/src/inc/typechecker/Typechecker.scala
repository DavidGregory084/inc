package inc.typechecker

import cats.data.Chain
import inc.common._

object Typechecker {
  type Environment = Map[String, TypeScheme]
  type Substitution = Map[TypeVariable, Type]

  def trace(name: String, typ: Type) = {
    println(name + ": " + Printer.print(typ))
  }

  def trace(name: String, typ: TypeScheme) = {
    println(name + ": " + Printer.print(typ))
  }

  def bind(tyVar: TypeVariable, typ: Type): Either[List[TypeError], Substitution] = typ match {
    case t @ TypeVariable(_) if tyVar == t =>
      Right(Map.empty)
    case t if tyVar.occursIn(t) =>
      TypeError.singleton("Attempt to construct infinite type")
    case _ =>
      Right(Map(tyVar -> typ))
  }

  def chainSubstitutions(ss: List[Substitution]): Substitution =
    ss.foldLeft(Map.empty: Substitution)(chainSubstitution)

  def chainSubstitutions(ss: Substitution*): Substitution =
    chainSubstitutions(ss.toList)

  def substitute(env: Environment, subst: Substitution): Environment =
    env.mapValues(_.substitute(subst))

  def chainSubstitution(s1: Substitution, s2: Substitution): Substitution =
    s2 ++ s1.mapValues(_.substitute(s2))

  def unify(left: Type, right: Type): Either[List[TypeError], Substitution] = {
    val ll = Printer.print(left)
    val rr = Printer.print(right)
    println(s"unify $ll with $rr")

    (left, right) match {
      case (TypeConstructor(_, lvars), TypeConstructor(_, rvars)) if lvars.length != rvars.length =>
        TypeError.singleton(s"Cannot unify $ll with $rr")
      case (TypeConstructor(l, lvars), TypeConstructor(r, rvars)) if l == r =>
        val emptyRes: Either[List[TypeError], Substitution] = Right(Map.empty)

        lvars.zip(rvars).foldLeft(emptyRes) {
          case (substSoFar, (ll, rr)) =>
            for {
              subst <- substSoFar
              newSubst <- unify(ll.substitute(subst), rr.substitute(subst))
            } yield chainSubstitution(subst, newSubst)
        }

       case (tyVar @ TypeVariable(_), typ) =>
         bind(tyVar, typ)

       case (typ, tyVar @ TypeVariable(_)) =>
         bind(tyVar, typ)

      case (_, _) =>
        TypeError.singleton(s"Cannot unify $ll with $rr")
    }
  }

  def typecheck(expr: Expr[Name], env: Environment): Either[List[TypeError], (Expr[NameWithType], Substitution)] = expr match {
    case int @ LiteralInt(_, _, _) =>
      Right((int.copy(meta = NameWithType(int.meta, TypeScheme(Type.Int))), Map.empty))
    case long @ LiteralLong(_, _, _) =>
      Right((long.copy(meta = NameWithType(long.meta, TypeScheme(Type.Long))), Map.empty))
    case float @ LiteralFloat(_, _, _) =>
      Right((float.copy(meta = NameWithType(float.meta, TypeScheme(Type.Float))), Map.empty))
    case double @ LiteralDouble(_, _, _) =>
      Right((double.copy(meta = NameWithType(double.meta, TypeScheme(Type.Double))), Map.empty))
    case bool @ LiteralBoolean(_, _, _) =>
      Right((bool.copy(meta = NameWithType(bool.meta, TypeScheme(Type.Boolean))), Map.empty))
    case char @ LiteralChar(_, _, _) =>
      Right((char.copy(meta = NameWithType(char.meta, TypeScheme(Type.Char))), Map.empty))
    case str @ LiteralString(_, _, _) =>
      Right((str.copy(meta = NameWithType(str.meta, TypeScheme(Type.String))), Map.empty))
    case unit @ LiteralUnit(_, _) =>
      Right((unit.copy(meta = NameWithType(unit.meta, TypeScheme(Type.Unit))), Map.empty))
    case ref @ Reference(name, _, _)  =>
      env.get(name).map { typ =>
        trace(s"reference to $name", typ)
        Right((ref.copy(meta = NameWithType(ref.meta, typ)), Map.empty: Substitution))
      }.getOrElse(TypeError.singleton(s"Reference to undefined symbol: $name"))

    case If(cond, thenExpr, elseExpr, nm, pos) =>
      for {
        // Typecheck the condition
        r1 <- typecheck(cond, env)
        (c, s1) = r1

        _ = trace("if condition", c.meta.typ)

        // Typecheck the then expression
        r2 <- typecheck(thenExpr, env)
        (t, s2) = r2

        _ = trace("then expression", t.meta.typ)

        // Typecheck the else expression
        r3 <- typecheck(elseExpr, env)
        (e, s3) = r3

        _ = trace("else expression", t.meta.typ)

        // Unify the condition with Boolean
        s4 <- unify(c.meta.typ.instantiate, Type.Boolean)

        // Unify the then expression and the else expression
        s5 <- unify(t.meta.typ.instantiate, e.meta.typ.instantiate)

        s = chainSubstitutions(s1, s2, s3, s4, s5)

        expr = If(c, t, e, NameWithType(nm, t.meta.typ), pos).substitute(s)

        _ = trace("if expression", expr.meta.typ)

      } yield (expr, s)

    case Lambda(variables, body, nm, pos) =>
      val tvs = variables.map(_ => TypeVariable())
      val tvMappings = variables.zip(tvs.map(TypeScheme(_)))

      tvMappings.foreach {
        case (v, tv) =>
          trace(v, tv)
      }

      for {
        // Typecheck the body with the variables in scope
        r <- typecheck(body, env ++ tvMappings)
        (b, s) = r

        _ = trace("lambda body", b.meta.typ)

        // Create a new function type
        tp = Type.Function(tvs, b.meta.typ.instantiate)

        // Apply the substitutions from the body
        ts = TypeScheme.generalize(env, tp.substitute(s))

        expr = Lambda(variables, b.substitute(s), NameWithType(nm, ts), pos)

        _ = trace("lambda expression", expr.meta.typ)

      } yield (expr, s)

    case Apply(fn, args, nm, pos) =>
      val tv = TypeVariable()

      trace("function application", tv)

      for {
        // Typecheck the function
        rf <- typecheck(fn, env)
        (f, s1) = rf

        _ = trace("function to apply", f.meta.typ)

        emptyRes: Either[List[TypeError], (Chain[Expr[NameWithType]], Substitution)] = Right((Chain.empty, Map.empty))

        // Typecheck the arg expressions
        ra <- args.zipWithIndex.foldLeft(emptyRes) {
          case (resSoFar, (nextArg, idx)) =>
            for {
              r <- resSoFar
              (typedSoFar, substSoFar) = r

              a <- typecheck(nextArg, substitute(env, s1))
              (typedArg, newSubst) = a

              _ = trace(s"argument ${idx + 1}", typedArg.meta.typ)

            } yield (typedSoFar :+ typedArg, chainSubstitution(substSoFar, newSubst))
        }

        (as, s2) = ra

        argsList = as.toList

        // Create a new function type
        tp = Type.Function(argsList.map(_.meta.typ.instantiate), tv).substitute(s2)

        _ = trace("expected type", tp)

        s3 <- unify(f.meta.typ.instantiate, tp)

        s = chainSubstitutions(s1, s2, s3)

        expr = Apply(f.substitute(s), argsList.map(_.substitute(s)), NameWithType(nm, TypeScheme(tv.substitute(s))), pos)

      } yield (expr, s2)
  }

  def typecheck(decl: TopLevelDeclaration[Name], env: Environment): Either[List[TypeError], (TopLevelDeclaration[NameWithType], Environment)] = decl match {
    case Let(name, expr, _, pos) =>
      typecheck(expr, env).flatMap {
        case (checkedExpr, subst) =>
          trace(name, checkedExpr.meta.typ)
          Right((Let(name, checkedExpr, NameWithType(decl.meta, checkedExpr.meta.typ), pos), substitute(env, subst).updated(name, checkedExpr.meta.typ)))
      }
  }

  def typecheck(
    module: Module[Name],
    importedDecls: Map[String, TopLevelDeclaration[NameWithType]] = Map.empty
  ): Either[List[TypeError], Module[NameWithType]] = module match {
    case Module(_, _, _, decls, _, _) =>

      val initialEnv = importedDecls.mapValues(_.meta.typ)

      val emptyDecls = Chain.empty[TopLevelDeclaration[NameWithType]]
      val emptyRes: Either[List[TypeError], (Chain[TopLevelDeclaration[NameWithType]], Environment)] = Right((emptyDecls, initialEnv))

      val typecheckedDecls = decls.foldLeft(emptyRes) {
        case (resSoFar, nextDecl) =>
          for {
            r <- resSoFar
            (checkedSoFar, envSoFar) = r
            c <- typecheck(nextDecl, envSoFar)
            (checkedDecl, updatedEnv) = c
          } yield (checkedSoFar :+ checkedDecl, updatedEnv)
      }

      typecheckedDecls.map {
        case (checked, _) =>
          module.copy(declarations = checked.toList, meta = NameWithType(module.meta, TypeScheme(Type.Module)))
      }
  }
}
