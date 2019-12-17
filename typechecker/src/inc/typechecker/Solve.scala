package inc.typechecker

import inc.common._

import cats.instances.either._
import cats.syntax.flatMap._
import com.typesafe.scalalogging.LazyLogging
import org.typelevel.paiges.Style
import scala.{ ::, Boolean, Left, Right, Nil, StringContext }
import scala.collection.immutable.{ List, Map }
import scala.Predef.ArrowAssoc

class Solve(context: Printer.SourceContext, isTraceEnabled: Boolean) extends LazyLogging {

  type Substitution = Map[TypeVariable, Type]
  val EmptySubst: Substitution = Map.empty

  def chainSubstitutions(ss: List[Substitution]): Substitution =
    ss.foldLeft(EmptySubst)(chainSubstitution)

  def chainSubstitutions(ss: Substitution*): Substitution =
    chainSubstitutions(ss.toList)

  def chainSubstitution(s1: Substitution, s2: Substitution): Substitution =
    s2 ++ s1.view.map { case (tyVar, typ) =>  (tyVar, typ.substitute(s2)) }.toMap

  def bind(pos: Pos, tyVar: TypeVariable, typ: Type): Infer[Substitution] =
    typ match {
      case t @ InferredTypeVariable(_, _) if tyVar == t =>
        Right(EmptySubst)
      case t @ NamedTypeVariable(_, _) if tyVar == t =>
        Right(EmptySubst)
      case t if tyVar.occursIn(t) =>
        TypeError.singleton(pos, "Attempt to construct infinite type")
      case t if tyVar.kind != t.kind =>
        TypeError.singleton(pos, s"Kinds do not match: ${Printer.print(tyVar.kind).render(80)} ${Printer.print(t.kind).render(80)}")
      case _ =>
        Right(Map(tyVar -> typ))
    }

  def unify(left: Type, right: Type, pos: Pos): Infer[Substitution] = {
    lazy val ll = Printer.print(left)
    lazy val rr = Printer.print(right)
    lazy val llRed = ll.style(Style.Ansi.Fg.Red).render(context.consoleWidth)
    lazy val rrRed = rr.style(Style.Ansi.Fg.Red).render(context.consoleWidth)
    lazy val llYellow = ll.style(Style.Ansi.Fg.Yellow).render(context.consoleWidth)
    lazy val rrYellow = rr.style(Style.Ansi.Fg.Yellow).render(context.consoleWidth)

    if (isTraceEnabled)
      logger.info(NL + s"Unify ${llYellow} with ${rrYellow}")

    def go(left: Type, right: Type): Infer[Substitution] = {
      (left, right) match {
        case (TypeApply(_, largs, _), TypeApply(_, rargs, _)) if largs.length != rargs.length =>
          TypeError.singleton(pos, s"${llRed} does not unify with ${rrRed}")

        case (TypeApply(ltyp, largs, _), TypeApply(rtyp, rargs, _)) =>
          unify(ltyp, rtyp, pos).flatMap { outerSubst =>

            val result: Infer[Substitution] = Right(outerSubst)

            largs.zip(rargs).foldLeft(result) {
              case (substSoFar, (ll, rr)) =>
                for {
                  subst <- substSoFar
                  newSubst <- unify(ll.substitute(subst), rr.substitute(subst), pos)
                } yield chainSubstitution(subst, newSubst)
            }
          }

        case (TypeConstructor(l, _), TypeConstructor(r, _)) if l == r =>
          Right(EmptySubst)

        case (tyVar @ InferredTypeVariable(_, _), typ) =>
          bind(pos, tyVar, typ)

        case (typ, tyVar @ InferredTypeVariable(_, _)) =>
          bind(pos, tyVar, typ)

        case (tyVar @ NamedTypeVariable(_, _), typ) =>
          bind(pos, tyVar, typ)

        case (typ, tyVar @ NamedTypeVariable(_, _)) =>
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
        unify(l, r, pos).map { subst =>
          Left((tail.map(_.substitute(subst)), chainSubstitution(substSoFar, subst)))
        }
    }
  }
}
