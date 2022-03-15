package inc.typechecker

import cats.Monoid
import cats.data.Chain
import cats.syntax.monoid._
import inc.common._

import scala.Predef.ArrowAssoc
import scala.collection.immutable.List
import scala.collection.immutable.Map

object Kindchecker {
  type Subst = Substitution[KindVariable, Kind]

  case class State(
    env: Environment[Meta.Typed],
    subst: Subst,
    errors: Chain[TypeError],
    constraints: List[KindConstraint]
  ) {
    def withEnv(newEnv: Environment[Meta.Typed]) =
      copy(env = newEnv)
    def withSubst(newSubst: Subst) =
      copy(subst = subst |+| newSubst)
    def withError(newError: TypeError) =
      copy(errors = errors :+ newError)
    def withConstraints(newConstraints: List[KindConstraint]) =
      copy(constraints = constraints ++ newConstraints)
  }
  object State {
    val empty = State(Environment.empty, Substitution.empty, Chain.empty, List.empty)
    def init(env: Environment[Meta.Typed]) = State(env, Substitution.empty, Chain.empty, List.empty)
    implicit def monoidForSolveState: Monoid[State] = new Monoid[State] {
      val empty: State =
        State.empty
      def combine(l: State, r: State): State =
        if (l.eq(State.empty))
          r
        else if (r.eq(State.empty))
          l
        else
          State(
            l.env ++ r.env,
            l.subst |+| r.subst,
            l.errors ++ r.errors,
            l.constraints ++ r.constraints
          )
    }
  }

  def gather(env: Environment[Meta.Typed], expr: TypeExpr[Meta.Typed]): List[KindConstraint] =
    expr match {
      case tyApp @ TypeApplyExpr(appliedTyp, args, meta) =>
        val typKind    = appliedTyp.meta.typ.typ.kind
        val resultKind = tyApp.meta.typ.typ.kind

        val tpCsts     = gather(env, appliedTyp)
        val tparamCsts = args.flatMap(gather(env, _))

        val tparamKinds = args.map(_.meta.typ.typ.kind)
        val appliedKind = Parameterized(tparamKinds, resultKind)

        val appCst =
          if (typKind != appliedKind)
            List(EqualKind(typKind, appliedKind, meta.pos))
          else
            List.empty

        tpCsts ++ tparamCsts ++ appCst

      case TypeConstructorExpr(name, meta) =>
        val typKind = meta.typ.typ.kind
        val envKind = env.kinds.getOrElse(name, typKind)

        val conCst =
          if (typKind != envKind)
            List(EqualKind(typKind, envKind, meta.pos))
          else
            List.empty

        conCst
    }

  def gather(env: Environment[Meta.Typed], constr: DataConstructor[Meta.Typed]): State =
    constr match {
      case DataConstructor(_, params, _) =>
        params.foldLeft(State.empty) { case (stateSoFar, Param(_, ascribedAs, meta)) =>
          val paramType      = meta.typ.typ
          val paramKind      = paramType.kind
          val paramResultCst = List(EqualKind(paramKind, Atomic, meta.pos))
          val paramCsts      = gather(env, ascribedAs.get)
          stateSoFar.withConstraints(paramResultCst ++ paramCsts)
        }
    }

  def gather(env: Environment[Meta.Typed], data: Data[Meta.Typed]): State =
    data match {
      case Data(_, _, cases, meta) =>
        val dataKind = data.kind

        val parentConstraint =
          if (meta.typ.typ.kind == dataKind)
            List.empty
          else
            List(EqualKind(meta.typ.typ.kind, dataKind, meta.pos))

        val constrState = cases.foldLeft(State.empty) { case (stateSoFar, nextConstr) =>
          stateSoFar |+| gather(env, nextConstr)
        }

        constrState.withConstraints(parentConstraint) |+| constrState
    }

  def bind(kindVar: KindVariable, kind: Kind, pos: Pos): State =
    kind match {
      case k @ KindVariable(_) if kindVar == k =>
        State.empty
      case k if kindVar.occursIn(k) =>
        State.empty.withError(TypeError.kindOccursCheck(pos, kindVar, kind))
      case _ =>
        State.empty.withSubst(Substitution(Map(kindVar -> kind)))
    }

  def unify(env: Environment[Meta.Typed], left: Kind, right: Kind, pos: Pos): State = {
    def go(left: Kind, right: Kind): State = {
      (left, right) match {
        case (Parameterized(lParams, _), Parameterized(rParams, _))
            if lParams.length != rParams.length =>
          State.empty.withError(TypeError.kindUnification(pos, left, right))

        case (Parameterized(lArgs, lRes), Parameterized(rArgs, rRes)) =>
          val paramState =
            lArgs.zip(rArgs).foldLeft(State.empty) { case (stateSoFar, (lArg, rArg)) =>
              unify(env, stateSoFar.subst(lArg), stateSoFar.subst(rArg), pos)
            }

          unify(env, paramState.subst(lRes), paramState.subst(rRes), pos)

        case (Atomic, Atomic) =>
          State.empty

        case (kindVar @ KindVariable(_), kind) =>
          bind(kindVar, kind, pos)

        case (kind, kindVar @ KindVariable(_)) =>
          bind(kindVar, kind, pos)

        case (_, _) =>
          State.empty.withError(TypeError.kindUnification(pos, left, right))
      }
    }

    go(left, right)
  }

  def solve(env: Environment[Meta.Typed], constraints: List[KindConstraint]): State = {
    constraints.foldLeft(State.empty) { case (currentState, nextConstraint) =>
      val EqualKind(l, r, pos) = currentState.subst(nextConstraint)
      val newState             = unify(env, l, r, pos)
      currentState |+| newState
    }
  }

  def kindcheck(data: Data[Meta.Typed], env: Environment[Meta.Typed]): State = {
    val gatherState = gather(env, data)

    val solveState = solve(env, gatherState.constraints)

    val updatedEnv = env
      .withKind(data.name, solveState.subst(data.kind))
      .substituteKinds(solveState.subst.subst)

    solveState.withEnv(updatedEnv)
  }
}
