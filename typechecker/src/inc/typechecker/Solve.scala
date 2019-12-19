package inc.typechecker

import inc.common._

import cats.instances.either._
import cats.syntax.flatMap._
import com.typesafe.scalalogging.LazyLogging
import org.typelevel.paiges._
import scala.{ ::, Boolean, Left, Right, Nil }
import scala.collection.immutable.{ List, Map }
import scala.Predef.ArrowAssoc

class Solve(context: Printer.SourceContext, isTraceEnabled: Boolean) extends LazyLogging {
  val kindChecker = new Kindchecker(context, isTraceEnabled)

  type Substitution = Map[TypeVariable, Type]
  val EmptySubst: Substitution = Map.empty

  def chainSubstitutions(ss: List[Substitution]): Substitution =
    ss.foldLeft(EmptySubst)(chainSubstitution)

  def chainSubstitutions(ss: Substitution*): Substitution =
    chainSubstitutions(ss.toList)

  def chainSubstitution(s1: Substitution, s2: Substitution): Substitution =
    s2 ++ s1.view.map { case (tyVar, typ) =>  (tyVar, typ.substitute(s2)) }.toMap

  def bind(tyVar: TypeVariable, typ: Type, pos: Pos): Infer[Substitution] =
    typ match {
      case t @ InferredTypeVariable(_, _, _) if tyVar == t =>
        Right(EmptySubst)
      case t @ NamedTypeVariable(_, _, _) if tyVar == t =>
        Right(EmptySubst)
      case t if tyVar.occursIn(t) =>
        TypeError.singleton(pos, "Attempt to construct infinite type")
      case t if tyVar.kind != t.kind =>
        kindChecker.unify(tyVar.kind, t.kind, pos).map { subst =>
          val updatedTyVar = tyVar.substituteKinds(subst).asInstanceOf[TypeVariable]
          val updatedTyp = t.substituteKinds(subst)
          Map(updatedTyVar -> updatedTyp)
        }
      case _ =>
        Right(Map(tyVar -> typ))
    }

  def unify(left: Type, right: Type, pos: Pos): Infer[Substitution] = {
    lazy val ll = Printer.print(left)
    lazy val rr = Printer.print(right)
    lazy val llRed = ll.style(Style.Ansi.Fg.Red)
    lazy val rrRed = rr.style(Style.Ansi.Fg.Red)
    lazy val llYellow = ll.style(Style.Ansi.Fg.Yellow)
    lazy val rrYellow = rr.style(Style.Ansi.Fg.Yellow)

    if (isTraceEnabled) {
      val traceMsg = Doc.hardLine + Doc.text("Unifying") & llYellow & Doc.text("with") & rrYellow
      logger.info(traceMsg.render(context.consoleWidth))
    }

    def go(left: Type, right: Type): Infer[Substitution] = {
      (left, right) match {
        case (TypeApply(_, largs, _, _), TypeApply(_, rargs, _, _)) if largs.length != rargs.length =>
          val errorMsg = llRed & Doc.text("does not unify with") & rrRed
          TypeError.singleton(pos, errorMsg.render(context.consoleWidth))

        case (TypeApply(ltyp, largs, _, _), TypeApply(rtyp, rargs, _, _)) =>
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

        case (TypeConstructor(l, _, _), TypeConstructor(r, _, _)) if l == r =>
          Right(EmptySubst)

        case (tyVar @ InferredTypeVariable(_, _, _), typ) =>
          bind(tyVar, typ, pos)

        case (typ, tyVar @ InferredTypeVariable(_, _, _)) =>
          bind(tyVar, typ, pos)

        case (tyVar @ NamedTypeVariable(_, _, _), typ) =>
          bind(tyVar, typ, pos)

        case (typ, tyVar @ NamedTypeVariable(_, _, _)) =>
          bind(tyVar, typ, pos)

        case (_, _) =>
          val errorMsg = llRed & Doc.text("does not unify with") & rrRed
          TypeError.singleton(pos, errorMsg.render(context.consoleWidth))
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
