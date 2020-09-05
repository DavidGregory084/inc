package inc.typechecker

import inc.common._

import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.monoid._
import com.typesafe.scalalogging.LazyLogging
import scala.{ ::, Left, Right, Nil }
import scala.collection.immutable.{ List, Map }
import scala.Predef.ArrowAssoc

object Solve extends LazyLogging {
  type Subst = Substitution[TypeVariable, Type]

  def bind(tyVar: TypeVariable, typ: Type, pos: Pos): Infer[Subst] =
    typ match {
      case t @ InferredTypeVariable(_, _) if tyVar == t =>
        Right(Substitution.empty)
      case t @ NamedTypeVariable(_, _) if tyVar == t =>
        Right(Substitution.empty)
      case t if tyVar.occursIn(t) =>
        TypeError.typeOccursCheck(pos, tyVar, typ)
      case t if tyVar.kind != t.kind =>
        Kindchecker.unify(tyVar.kind, t.kind, pos).map { subst =>
          val updatedTyVar = tyVar.substituteKinds(subst.subst).asInstanceOf[TypeVariable]
          val updatedTyp = t.substituteKinds(subst.subst)
          Substitution(Map(updatedTyVar -> updatedTyp))
        }.leftMap {
          case KindUnificationError(pos, _, _) :: rest =>
            TypeApplicationError(pos, tyVar, t) :: rest
          case other =>
            other
        }
      case _ =>
        Right(Substitution(Map(tyVar -> typ)))
    }

  def unify(left: Type, right: Type, pos: Pos): Infer[Subst] = {
    def go(left: Type, right: Type): Infer[Subst] = {
      (left, right) match {
        case (Type.Function(largs), Type.Function(rargs)) if largs.length != rargs.length =>
          TypeError.typeUnification(pos, left, right)

        case (TypeApply(ltyp, largs, _), TypeApply(rtyp, rargs, _)) =>
          unify(ltyp, rtyp, pos).flatMap { outerSubst =>

            val result: Infer[Subst] = Right(outerSubst)

            largs.zip(rargs).foldLeft(result) {
              case (substSoFar, (ll, rr)) =>
                for {
                  subst <- substSoFar
                  newSubst <- unify(ll.substitute(subst.subst), rr.substitute(subst.subst), pos)
                } yield Substitution.chain(subst, newSubst)
            }
          }

        case (TypeConstructor(l, _), TypeConstructor(r, _)) if l == r =>
          Right(Substitution.empty)

        case (tyVar @ InferredTypeVariable(_, _), typ) =>
          bind(tyVar, typ, pos)

        case (typ, tyVar @ InferredTypeVariable(_, _)) =>
          bind(tyVar, typ, pos)

        case (tyVar @ NamedTypeVariable(_, _), typ) =>
          bind(tyVar, typ, pos)

        case (typ, tyVar @ NamedTypeVariable(_, _)) =>
          bind(tyVar, typ, pos)

        case (_, _) =>
          TypeError.typeUnification(pos, left, right)
      }
    }

    go(left, right)
  }

  def solve(constraints: List[TypeConstraint]): Infer[Subst] = {
    (constraints, Substitution.empty: Subst).tailRecM {
      case (Nil, subst) =>
        Right(Right(subst))
      case (EqualType(l, r, pos) :: tail, substSoFar) =>
        unify(l, r, pos).map { subst =>
          Left((tail.map(_.substitute(subst.subst)), substSoFar |+| subst))
        }
    }
  }
}
