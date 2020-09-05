package inc.typechecker

import inc.common._
import cats.syntax.foldable._
import cats.syntax.flatMap._
import cats.syntax.monoid._
import scala.{ ::, Left, Right, Nil }
import scala.collection.immutable.{ List, Map }
import scala.Predef.{ ???, ArrowAssoc }
import com.typesafe.scalalogging.LazyLogging

object Kindchecker extends LazyLogging {
  type Subst = Substitution[KindVariable, Kind]

  def gather(typ: TypeExpr[Meta.Typed], env: Environment[Meta.Typed]): List[KindConstraint] = ???

  def gather(constr: DataConstructor[Meta.Typed], env: Environment[Meta.Typed]): List[KindConstraint] = ???

  def gather(data: Data[Meta.Typed], env: Environment[Meta.Typed]): Infer[List[KindConstraint]] = ???

  def bind(kindVar: KindVariable, kind: Kind, pos: Pos): Infer[Subst] =
    kind match {
      case k @ KindVariable(_) if kindVar == k =>
        Right(Substitution.empty)
      case k if kindVar.occursIn(k) =>
        TypeError.kindOccursCheck(pos, kindVar, kind)
      case _ =>
        Right(Substitution(Map(kindVar -> kind)))
    }

  def unify(left: Kind, right: Kind, pos: Pos): Infer[Subst] = {
    def go(left: Kind, right: Kind): Infer[Subst] = {
      (left, right) match {
        case (Parameterized(lParams, _), Parameterized(rParams, _)) if lParams.length != rParams.length =>
          TypeError.kindUnification(pos, left, right)

        case (Parameterized(lArgs, lRes), Parameterized(rArgs, rRes)) =>
          lArgs.zip(rArgs).foldM(Substitution.empty: Subst) {
            case (subst, (lArg, rArg)) =>
              unify(subst(lArg), subst(rArg), pos).map { subst |+| _ }
          }.flatMap { paramSubst =>
            unify(paramSubst(lRes), paramSubst(rRes), pos).map { paramSubst |+| _ }
          }

        case (Atomic, Atomic) =>
          Right(Substitution.empty)

        case (kindVar @ KindVariable(_), kind) =>
          bind(kindVar, kind, pos)

        case (kind, kindVar @ KindVariable(_)) =>
          bind(kindVar, kind, pos)

        case (_, _) =>
          TypeError.kindUnification(pos, left, right)
      }
    }

    go(left, right)
  }

  def solve(constraints: List[KindConstraint]): Infer[Subst] = {
    (constraints, Substitution.empty: Subst).tailRecM {
      case (Nil, subst) =>
        Right(Right(subst))
      case (EqualKind(l, r, pos) :: tail, substSoFar) =>
        unify(l, r, pos).map { subst =>
          Left((tail.map(subst(_)), substSoFar |+| subst))
        }
    }
  }

  def kindcheck(data: Data[Meta.Typed], env: Environment[Meta.Typed]): Infer[(Environment[Meta.Typed], Subst)] = {
    for {
      csts <- gather(data, env)

      subst <- solve(csts)

      updatedEnv = data.meta.typ.bound
        .map(tv => tv.name -> subst(tv.kind))
        .toMap.updated(data.name, subst(data.kind))

    } yield (env.withKinds(updatedEnv), subst)
  }
}
