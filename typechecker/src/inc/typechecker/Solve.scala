package inc.typechecker

import inc.common._

import cats.instances.either._
// import cats.syntax.either._
import cats.syntax.flatMap._
import scala.{ ::, Boolean, Left, Right, Some, Nil, StringContext }
import scala.collection.immutable.{ List, Map }
import scala.Predef.ArrowAssoc
import scribe._
import scribe.format._

class Solve(isTraceEnabled: Boolean) {
  if (isTraceEnabled) {
    this.logger.withHandler(
      formatter = Formatter.simple,
      minimumLevel = Some(Level.Trace)
    ).replace()
  }

  def bind(pos: Pos, tyVar: TypeVariable, typ: Type): Infer[Substitution] =
    typ match {
      case t @ TypeVariable(_) if tyVar == t =>
        Right(EmptySubst)
      case t if tyVar.occursIn(t) =>
        TypeError.singleton(pos, "Attempt to construct infinite type")
      case _ =>
        Right(Map(tyVar -> typ))
    }

  def unify(pos: Pos, left: Type, right: Type): Infer[Substitution] = {
    lazy val ll = Printer.print(left)
    lazy val rr = Printer.print(right)
    lazy val llRed = Red(ll)
    lazy val rrRed = Red(rr)

    scribe.trace(NL + s"Unify ${Yellow(ll)} with ${Yellow(rr)}")

    def go(left: Type, right: Type): Infer[Substitution] = {
      (left, right) match {
        case (TypeConstructor(_, lvars), TypeConstructor(_, rvars)) if lvars.length != rvars.length =>
          TypeError.singleton(pos, s"${llRed} does not unify with ${rrRed}")

        case (TypeConstructor(l, lvars), TypeConstructor(r, rvars)) if l == r =>
          val emptyRes: Infer[Substitution] = Right(EmptySubst)

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

    go(left, right)
  }

  def solve(constraints: List[Constraint]): Infer[Substitution] = {
    (constraints, EmptySubst).tailRecM {
      case (Nil, subst) =>
        Right(Right(subst))
      case (Equal(l, r, pos) :: tail, substSoFar) =>
        unify(pos, l, r).map { subst =>
          Left((tail, chainSubstitution(substSoFar, subst)))
        }
    }
  }
}
