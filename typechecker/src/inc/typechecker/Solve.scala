package inc.typechecker

import inc.common._

import cats.Monoid
import cats.syntax.monoid._
import scala.collection.immutable.{ List, Map }
import scala.Predef.ArrowAssoc
import cats.data.Chain

object Solve {
  type Subst = Substitution[TypeVariable, Type]

  case class State(
    env: Environment[Meta.Typed],
    subst: Subst,
    errors: Chain[TypeError]
  ) {
    def withSubst(newSubst: Subst) =
      copy(subst = subst |+| newSubst)
    def withErrors(newErrors: Chain[TypeError]) =
      copy(errors = errors ++ newErrors)
    def withError(newError: TypeError) =
      copy(errors = errors :+ newError)
  }
  object State {
    val empty = State(Environment.empty, Substitution.empty, Chain.empty)
    def init(env: Environment[Meta.Typed]) = State(env, Substitution.empty, Chain.empty)
    implicit def monoidForSolveState: Monoid[State] = new Monoid[State] {
      def empty: State = State.init(Environment.empty)
      def combine(l: State, r: State): State =
        State(l.env ++ r.env, l.subst |+| r.subst, l.errors ++ r.errors)
    }
  }

  def bind(env: Environment[Meta.Typed], tyVar: TypeVariable, typ: Type, pos: Pos): State =
    typ match {
      case t @ InferredTypeVariable(_, _) if tyVar == t =>
        State.empty
      case t @ NamedTypeVariable(_, _) if tyVar == t =>
        State.empty
      case t if tyVar.occursIn(t) =>
        State.empty.withError(TypeError.typeOccursCheck(pos, tyVar, typ))
      case t if tyVar.kind != t.kind =>
        val kindState = Kindchecker.unify(env, tyVar.kind, t.kind, pos)
        val updatedTyVar = tyVar.substituteKinds(kindState.subst.subst)
        val updatedTyp = t.substituteKinds(kindState.subst.subst)
        val newSubst = Substitution(Map(updatedTyVar -> updatedTyp))
        State.empty.withSubst(newSubst).withErrors(kindState.errors)
      case _ =>
        State.empty.withSubst(Substitution(Map(tyVar -> typ)))
    }

  def unify(env: Environment[Meta.Typed], left: Type, right: Type, pos: Pos): State = {
    def go(left: Type, right: Type): State = {
      (left, right) match {
        case (Type.Function(largs), Type.Function(rargs)) if largs.length != rargs.length =>
          State.empty.withError(TypeError.typeUnification(pos, left, right))

        case (TypeApply(ltyp, largs, _), TypeApply(rtyp, rargs, _)) =>
            val tyConState = unify(env, ltyp, rtyp, pos)

            largs.zip(rargs).foldLeft(tyConState) {
              case (stateSoFar, (ll, rr)) =>
                val argState = unify(env, stateSoFar.subst(ll), stateSoFar.subst(rr), pos)
                stateSoFar |+| argState
            }

        case (TypeConstructor(l, _), TypeConstructor(r, _)) if l == r =>
          State.empty

        case (tyVar @ InferredTypeVariable(_, _), typ) =>
          bind(env, tyVar, typ, pos)

        case (typ, tyVar @ InferredTypeVariable(_, _)) =>
          bind(env, tyVar, typ, pos)

        case (tyVar @ NamedTypeVariable(_, _), typ) =>
          bind(env, tyVar, typ, pos)

        case (typ, tyVar @ NamedTypeVariable(_, _)) =>
          bind(env, tyVar, typ, pos)

        case (_, _) =>
          State.empty.withError(TypeError.typeUnification(pos, left, right))
      }
    }

    go(left, right)
  }

  def solve(env: Environment[Meta.Typed], constraints: List[TypeConstraint]): State = {
    constraints.foldLeft(State.init(env)) {
      // Ignore constraints arising from type errors
      case (currentState, nextConstraint) if nextConstraint.containsError =>
        currentState
      case (currentState, nextConstraint) =>
        val EqualType(l, r, pos) = currentState.subst(nextConstraint)
        val newState = unify(env, l, r, pos)
        currentState |+| newState
    }
  }
}
