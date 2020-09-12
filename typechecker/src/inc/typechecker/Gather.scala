package inc.typechecker

import cats.data.Chain
import cats.Monoid
import cats.syntax.monoid._
import inc.common._
import scala.Predef.{ ???, ArrowAssoc }
import scala.{ Some, None, StringContext }
import scala.collection.immutable.List

object Gather {
  case class State(
    env: Environment[Meta.Typed],
    errors: Chain[TypeError],
    constraints: List[TypeConstraint]
  ) {
    def withEnv(newEnv: Environment[Meta.Typed]) =
      copy(env = newEnv)
    def withError(newError: TypeError) =
      copy(errors = errors :+ newError)
    def withErrors(newErrors: Chain[TypeError]) =
      copy(errors = errors ++ newErrors)
    def withConstraint(newConstraint: TypeConstraint) =
      copy(constraints = constraints ++ List(newConstraint))
    def withConstraints(newConstraints: List[TypeConstraint]) =
      copy(constraints = constraints ++ newConstraints)
  }
  object State {
    val empty = State(Environment.empty, Chain.empty, List.empty)
    def init(env: Environment[Meta.Typed]) = State(env, Chain.empty, List.empty)
    implicit def monoidForGatherState: Monoid[State] = new Monoid[State] {
      def empty: State = State.empty
      def combine(l: State, r: State): State =
        State(l.env ++ r.env, l.errors ++ r.errors, l.constraints ++ r.constraints)
    }
  }

  def gather(
    typExpr: TypeConstructorExpr[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): (TypeConstructorExpr[Meta.Typed], State) = typExpr match {
    case TypeConstructorExpr(name, meta) =>
      env.types.get(name).map { typ =>
        val checkedTypExpr = TypeConstructorExpr(name, meta.withType(typ))
        (checkedTypExpr, State.empty)
      }.getOrElse {
        val checkedTypExpr = TypeConstructorExpr(name, meta.withSimpleType(ErrorType))
        val typeError = TypeError.generic(meta.pos, s"Reference to undefined type: ${name}")
        (checkedTypExpr, State.empty.withError(typeError))
      }
  }

  def gather(
    typExpr: TypeExpr[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): (TypeExpr[Meta.Typed], State) = typExpr match {

    case tyCon @ TypeConstructorExpr(_, _) =>
      gather(tyCon, env)

    case TypeApplyExpr(typ, args, meta) =>
      val (checkedTyp, typState) = gather(typ, env)

      val initialArgState = (Chain.empty[TypeExpr[Meta.Typed]], State.init(env))
      val (checkedArgs, argsState) = args.foldLeft(initialArgState) {
        case (state @ (_, stateSoFar), nextArg) =>
          val (arg, argState) = gather(nextArg, stateSoFar.env)
          val newState = (Chain.one(arg), argState)
          state |+| newState
      }

      val tyArgTypes = checkedArgs.map(_.meta.typ.typ)
      val tyAppType = TypeApply(checkedTyp.meta.typ.typ, tyArgTypes.toList, Atomic)

      val checkedTypeExpr = TypeApplyExpr(
        typ = checkedTyp,
        args = checkedArgs.toList,
        meta = meta.withSimpleType(tyAppType)
      )

      (checkedTypeExpr, typState |+| argsState)
  }

  def gather(
    param: Param[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): (Chain[Param[Meta.Typed]], State) = param match {
    case Param(_, Some(ascribedAs), meta) =>
      val (asc, ascState) = gather(ascribedAs, env)

      val checkedParam = param.copy(
        ascribedAs = Some(asc),
        meta = meta.withType(asc.meta.typ))

      (Chain.one(checkedParam), ascState)

    case Param(_, None, meta) =>
      val checkedParam = param.copy(
        ascribedAs = None,
        meta = meta.withSimpleType(TypeVariable()))

      (Chain.one(checkedParam), State.empty)
  }

  def gather(
    expr: Expr[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): (Expr[Meta.Typed], State) = expr match {
    case int @ LiteralInt(_, _) =>
      (int.copy(meta = int.meta.withSimpleType(Type.Int)), State.empty)

    case long @ LiteralLong(_, _) =>
      (long.copy(meta = long.meta.withSimpleType(Type.Long)), State.empty)

    case float @ LiteralFloat(_, _) =>
      (float.copy(meta = float.meta.withSimpleType(Type.Float)), State.empty)

    case double @ LiteralDouble(_, _) =>
      (double.copy(meta = double.meta.withSimpleType(Type.Double)), State.empty)

    case boolean @ LiteralBoolean(_, _) =>
      (boolean.copy(meta = boolean.meta.withSimpleType(Type.Boolean)), State.empty)

    case char @ LiteralChar(_, _) =>
      (char.copy(meta = char.meta.withSimpleType(Type.Char)), State.empty)

    case string @ LiteralString(_, _) =>
      (string.copy(meta = string.meta.withSimpleType(Type.String)), State.empty)

    case unit @ LiteralUnit(_) =>
      (unit.copy(meta = unit.meta.withSimpleType(Type.Unit)), State.empty)

    case ref @ Reference(_, _, meta) =>
      env.types.get(ref.fullName).map { refTyp =>
        val instTyp = refTyp.instantiate
        val checkedRef = ref.copy(meta = meta.withSimpleType(instTyp))
        (checkedRef, State.empty)
      }.getOrElse {
        val checkedRef = ref.copy(meta = meta.withSimpleType(ErrorType))
        val typeError = TypeError.generic(meta.pos, s"Reference to undefined symbol: ${ref.fullName}")
        (checkedRef, State.empty.withError(typeError))
      }

    case Ascription(expr, ascribedAs, meta) =>
      val (checkedExpr, exprState) = gather(expr, env)

      val exprType = checkedExpr.meta.typ.typ

      val (checkedTyExp, tyExpState) = gather(ascribedAs, env)

      val ascribedType = checkedTyExp.meta.typ.typ

      val ascriptionCst =
        // Unless we know the ascription is correct
        if (exprType == ascribedType)
          List.empty
        // Emit a constraint that the expression's type must match the ascription
        else
          List(EqualType(exprType, ascribedType, meta.pos))

      val checkedAsc = Ascription(
        checkedExpr,
        checkedTyExp,
        meta.withSimpleType(ascribedType)
      )

      val newState = (exprState |+| tyExpState).withConstraints(ascriptionCst)

      (checkedAsc, newState)

    case If(cond, thenExpr, elseExpr, meta) =>
      val (c, cState) = gather(cond, env)
      val Meta.Typed(_, condType, condPos) = c.meta

      val (t, tState) = gather(thenExpr, env)
      val Meta.Typed(_, thenType, _) = t.meta

      val (e, eState) = gather(elseExpr, env)
      val Meta.Typed(_, elseType, _) = e.meta

      val condBoolean = EqualType(condType.typ, Type.Boolean, condPos)
      val thenElseEqual = EqualType(thenType.typ, elseType.typ, meta.pos)

      val checkedExpr = If(c, t, e, meta.withType(thenType))
      val constraints = List(condBoolean, thenElseEqual)

      val newState = (cState |+| tState |+| eState).withConstraints(constraints)

      (checkedExpr, newState)

    case Lambda(params, body, meta) =>
      val initialState = (Chain.empty[Param[Meta.Typed]], State.init(env))
      val (checkedParams, paramState) = params.foldLeft(initialState) {
        case (state @ (_, stateSoFar), nextParam) =>
          state |+| gather(nextParam, stateSoFar.env)
      }

      val paramMappings = checkedParams.toList.map(p => p.name -> p.meta.typ)
      val paramTypes = checkedParams.toList.map(_.meta.typ.typ)

      val (checkedBody, bodyState) = gather(body, env.withTypes(paramMappings))
      val Meta.Typed(_, bodyType, _) = checkedBody.meta

      val funType = Type.Function(paramTypes, bodyType.typ)

      val expr = Lambda(checkedParams.toList, checkedBody, meta.withSimpleType(funType))

      val newState = (paramState |+| bodyState)

      (expr, newState)

    case Apply(fn, args, meta) =>
      val tv = TypeVariable()

      val (checkedFn, fnState) = gather(fn, env)

      val initialArgState = (Chain.empty[Expr[Meta.Typed]], State.init(env))
      val (checkedArgs, argState) = args.foldLeft(initialArgState) {
        case (state @ (_, stateSoFar), nextArg) =>
          val (arg, argState) = gather(nextArg, stateSoFar.env)
          val newState = (Chain.one(arg), argState)
          state |+| newState
      }

      val declaredType = checkedFn.meta.typ.typ
      val argTypes = checkedArgs.toList.map(_.meta.typ.typ)
      val appliedType = Type.Function(argTypes, tv)

      val declEqualsAppCst = EqualType(declaredType, appliedType, meta.pos)

      val checkedExpr = Apply(
        checkedFn,
        checkedArgs.toList,
        meta.withSimpleType(tv)
      )

      val newState = (fnState |+| argState).withConstraint(declEqualsAppCst)

      (checkedExpr, newState)
  }

  def gather(
    decl: TopLevelDeclaration[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): (Chain[TopLevelDeclaration[Meta.Typed]], State) = decl match {
    case let @ Let(name, expr, meta) =>
      val (checkedExpr, exprState) = gather(expr, env)

      // We have to solve the constraints under a `let` before we generalize it
      val Solve.State(_, subst, solveErrors) = Solve.solve(env, exprState.constraints)

      // Otherwise, the constraint substitution could not be applied to any bound type variables
      val solvedExpr = subst(checkedExpr)

      val exprType = TypeScheme.generalize(env, solvedExpr.meta.typ.typ)
      val checkedLet = Chain.one(let.copy(binding = solvedExpr, meta = meta.withType(exprType)))

      val newState = State.empty
        .withErrors(exprState.errors)
        .withErrors(solveErrors)
        .withEnv(env.withType(name, exprType))

      (checkedLet, newState)

    case Data(name, typeParams, cases, meta) =>
      ???
  }

  def gather(
    module: Module[Meta.Untyped],
    importedEnv: Environment[Meta.Typed]
  ): (Module[Meta.Typed], State) = {
    val initialState = (Chain.empty[TopLevelDeclaration[Meta.Typed]], State.init(importedEnv))

    val (decls, finalState) = module.declarations.foldLeft(initialState) {
      case (state @ (_, stateSoFar), nextDecl) =>
        state |+| gather(nextDecl, stateSoFar.env)
    }

    val updatedMod = module.copy(
      declarations = decls.toList,
      meta = module.meta.withSimpleType(Type.Module)
    )

    (updatedMod, finalState)
  }
}
