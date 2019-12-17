package inc.typechecker

import inc.common._
import java.lang.String
import cats.instances.either._
import cats.syntax.flatMap._
import org.typelevel.paiges._
import scala.{ ::, Left, Right, Nil, StringContext }
import scala.collection.immutable.{ List, Map }
import scala.Predef.{ ArrowAssoc, augmentString }
import com.typesafe.scalalogging.LazyLogging

object Kindchecker extends LazyLogging {
  type Substitution = Map[KindVariable, Kind]
  val EmptySubst: Substitution = Map.empty

  def trace(name: String, kind: Kind) = {
    val kindString = Printer.print(kind).render(80)
    val formattedMsg = NL + name + ": " + kindString
    logger.info(formattedMsg)
  }

  def trace(name: String, env: Map[String, Kind]) = {
    val formattedMsg = NL + name + ": " + (NL * 2) + env.map {
      case (nm, kind) =>
        val kindStr = Printer.print(kind).render(80)
        nm + ": " + kindStr
    }.mkString(NL)

    logger.info(formattedMsg)
  }

  def trace(name: String, constraint: KindConstraint) = {
    val formattedMsg = NL + name + ": " + Printer.print(constraint).render(80)
    logger.info(formattedMsg)
  }

  def trace(name: String, constraints: List[KindConstraint]) = {
    if (constraints.nonEmpty) {
      val formattedMsg = NL + name + ": " + (NL * 2) +
        constraints.map(Printer.print).map(_.render(80)).mkString(NL)
      logger.info(formattedMsg)
    }
  }

  def gather(typ: Type, pos: Pos): List[KindConstraint] = typ match {
    case NamedTypeVariable(_, _) =>
      List.empty
    case InferredTypeVariable(_, _) =>
      List.empty
    case TypeConstructor(_, _) =>
      List.empty
    case TypeApply(tp, appliedTps) =>
      val tparamKinds = appliedTps.map(_.kind)
      val appKind = Parameterized(tparamKinds, Atomic)
      val appCst = List(EqualKind(tp.kind, appKind, pos))
      val tparamCsts = appliedTps.flatMap(gather(_, pos))
      val tpString = Printer.print(tp).render(80)
      trace("Application of " + tpString, appCst)
      tparamCsts ++ appCst
  }

  def gather(constr: DataConstructor[NamePosType]): List[KindConstraint] = constr match {
    case DataConstructor(_, params, _, _) =>
      params.foldLeft(List.empty[KindConstraint]) {
        case (cstsSoFar, Param(_, _, meta)) =>
          cstsSoFar ++ gather(meta.typ.typ, meta.pos)
      }
  }

  def gather(data: Data[NamePosType]): Infer[List[KindConstraint]] = data match {
    case Data(name, _, cases, meta) =>
      val parentKind = KindVariable()
      val parentConstraints = List(EqualKind(parentKind, data.kind, meta.pos))

      val constraintsFromConstrs = cases.foldLeft(List.empty[KindConstraint]) {
        case (cstsSoFar, nextConstr) =>
          cstsSoFar ++ gather(nextConstr)
      }

      trace(name, constraintsFromConstrs ++ parentConstraints)

      Right(constraintsFromConstrs ++ parentConstraints)
  }

  def chainSubstitutions(ss: List[Substitution]): Substitution =
    ss.foldLeft(EmptySubst)(chainSubstitution)

  def chainSubstitutions(ss: Substitution*): Substitution =
    chainSubstitutions(ss.toList)

  def chainSubstitution(s1: Substitution, s2: Substitution): Substitution =
    s2 ++ s1.view.map { case (kindVar, kind) =>  (kindVar, kind.substitute(s2)) }.toMap

  def bind(pos: Pos, kindVar: KindVariable, kind: Kind): Infer[Substitution] =
    kind match {
      case k @ KindVariable(_) if kindVar == k =>
        Right(EmptySubst)
      case k if kindVar.occursIn(k) =>
        TypeError.singleton(pos, "Attempt to construct infinite kind")
      case _ =>
        Right(Map(kindVar -> kind))
    }

  def unify(left: Kind, right: Kind, pos: Pos): Infer[Substitution] = {
    lazy val ll = Printer.print(left)
    lazy val rr = Printer.print(right)
    lazy val llRed = ll.style(Style.Ansi.Fg.Red).render(80)
    lazy val rrRed = rr.style(Style.Ansi.Fg.Red).render(80)
    lazy val llYellow = ll.style(Style.Ansi.Fg.Yellow).render(80)
    lazy val rrYellow = ll.style(Style.Ansi.Fg.Yellow).render(80)

    logger.info(NL + s"Unify ${llYellow} with ${rrYellow}")

    def go(left: Kind, right: Kind): Infer[Substitution] = {
      (left, right) match {
        case (Parameterized(lparams, _), Parameterized(rparams, _)) if lparams.length != rparams.length =>
          TypeError.singleton(pos, s"${llRed} does not unify with ${rrRed}")

        case (Parameterized(largs, _), Parameterized(rargs, _)) =>
          val emptyRes: Infer[Substitution] = Right(EmptySubst)

          largs.zip(rargs).foldLeft(emptyRes) {
            case (substSoFar, (ll, rr)) =>
              for {
                subst <- substSoFar
                newSubst <- unify(ll.substitute(subst), rr.substitute(subst), pos)
              } yield chainSubstitution(subst, newSubst)
          }

        case (Atomic, Atomic) =>
          Right(EmptySubst)

        case (kindVar @ KindVariable(_), kind) =>
          bind(pos, kindVar, kind)

        case (kind, kindVar @ KindVariable(_)) =>
          bind(pos, kindVar, kind)

        case (_, _) =>
          TypeError.singleton(pos, s"${llRed} does not unify with ${rrRed}")
      }
    }

    go(left, right)
  }

  def solve(constraints: List[KindConstraint]): Infer[Substitution] = {
    (constraints, EmptySubst).tailRecM {
      case (Nil, subst) =>
        Right(Right(subst))
      case (EqualKind(l, r, pos) :: tail, substSoFar) =>
        unify(l, r, pos).map { subst =>
          Left((tail.map(_.substitute(subst)), chainSubstitution(substSoFar, subst)))
        }
    }
  }

  def kindcheck(data: Data[NamePosType]): Infer[Data[NamePosType]] = {
    for {
      csts <- gather(data)

      subst <- solve(csts)

      _ = if (subst.nonEmpty) {
        val substitution = Printer.printKindSubst(subst).render(80)
        logger.info(NL + "Apply substitution: " + substitution)
      }

      checkedData = data
        .substituteKinds(subst)
        .defaultKinds

      kindEnv = checkedData.typeParams
        .map(tv => tv.name -> tv.kind)
        .toMap.updated(checkedData.name, checkedData.kind)

      _ = if (kindEnv.nonEmpty) {
        trace("Final kind environment", kindEnv)
      }

    } yield checkedData
  }
}
