package inc.typechecker

import inc.common._
import java.lang.String
import cats.instances.either._
import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.flatMap._
import org.typelevel.paiges._
import scala.{ ::, Boolean, Left, Right, Nil, StringContext }
import scala.collection.immutable.{ List, Map }
import scala.Predef.ArrowAssoc
import com.typesafe.scalalogging.LazyLogging

class Kindchecker(context: Printer.SourceContext, isTraceEnabled: Boolean) extends LazyLogging {
  type Substitution = Map[KindVariable, Kind]
  val EmptySubst: Substitution = Map.empty

  def highlightSource(msg: String, pos: Pos): String =
    Printer.withSourceContext(context)(msg, pos, Style.Ansi.Fg.Yellow)

  def trace(name: String, kind: Kind, pos: Pos) = {
    if (isTraceEnabled) {
      val formattedMsg = Doc.hardLine + Doc.text(name + ":") & Printer.print(kind)
      val formattedStr = formattedMsg.render(context.consoleWidth)
      logger.info(highlightSource(formattedStr, pos))
    }
  }

  def trace(name: String, env: Map[String, Kind]) = {
    if (env.nonEmpty && isTraceEnabled) {
      val header = Doc.hardLine + Doc.text(name + ":") + (Doc.hardLine * 2)

      val formattedMsg = header + Doc.intercalate(Doc.hardLine, env.map {
        case (nm, kind) =>
          val kindStr = Printer.print(kind)
          Doc.text(nm + ":") & kindStr
      })

      val formattedStr = formattedMsg.render(context.consoleWidth)

      logger.info(formattedStr)
    }
  }

  def trace(name: String, constraint: KindConstraint) = {
    if (isTraceEnabled) {
      val formattedMsg = Doc.hardLine + Doc.text(name + ":") & Printer.print(constraint)
      val formattedStr = formattedMsg.render(context.consoleWidth)
      logger.info(highlightSource(formattedStr, constraint.pos))
    }
  }

  def trace(name: String, constraints: List[KindConstraint]) = {
    if (constraints.nonEmpty && isTraceEnabled) {
      val header = Doc.hardLine + Doc.text(name + ":") & (Doc.hardLine * 2)
      val formattedMsg = header + Doc.intercalate(Doc.hardLine, constraints.map(Printer.print))
      val formattedStr = formattedMsg.render(context.consoleWidth)
      logger.info(formattedStr)
    }
  }

  def gather(typ: Type, env: Environment[Meta.Typed]): List[KindConstraint] = typ match {
    case NamedTypeVariable(nm, kind, pos) =>
      trace(s"Reference to $nm", kind, pos)
      List.empty

    case InferredTypeVariable(_, _, _) =>
      List.empty

    case TypeConstructor(nm, kind, pos) =>
      trace(s"Reference to $nm", kind, pos)

      val envKind = env.kinds.getOrElse(nm, kind)

      if (kind != envKind)
        List(EqualKind(kind, envKind, pos))
      else
        List.empty

    case TypeApply(tp, appliedTps, kind, pos) =>
      trace("Type application", kind, pos)

      val tpCsts = gather(tp, env)
      trace("Type to apply", tpCsts)

      val tparamCsts = appliedTps.flatMap(gather(_, env))

      tparamCsts.zipWithIndex.foreach {
        case (csts, idx) =>
          trace(s"Type argument ${idx + 1}", csts)
      }

      val tparamKinds = appliedTps.map(_.kind)
      val appliedKind = Parameterized(tparamKinds, kind)

      val appCst = List(EqualKind(tp.kind, appliedKind, pos))

      trace("Applied kind", appCst)

      tpCsts ++ tparamCsts ++ appCst
  }

  def gather(constr: DataConstructor[Meta.Typed], env: Environment[Meta.Typed]): List[KindConstraint] = constr match {
    case DataConstructor(name, params, _, _) =>
      val constraints = params.foldLeft(List.empty[KindConstraint]) {
        case (cstsSoFar, Param(paramName, _, meta)) =>
          val paramResultCst = EqualKind(meta.typ.typ.kind, Atomic, meta.pos)
          trace(paramName, paramResultCst)
          val paramCsts = gather(meta.typ.typ, env)
          cstsSoFar ++ List(paramResultCst) ++ paramCsts
      }

      trace(name, constraints)

      constraints
  }

  def gather(data: Data[Meta.Typed], env: Environment[Meta.Typed]): Infer[List[KindConstraint]] = data match {
    case Data(name, tparams, cases, meta) =>
      val kind =
        if (tparams.isEmpty) {
          val TypeConstructor(_, tyConKind, _) = meta.typ.typ
          tyConKind
        } else {
          val TypeApply(TypeConstructor(_, tyConKind, _), _, _, _) = meta.typ.typ
          tyConKind
        }

      val parentConstraint =
        if (kind == data.kind)
          List.empty
        else
          List(EqualKind(kind, data.kind, meta.pos))

      tparams.foreach { tparam =>
        trace(tparam.name, tparam.kind, tparam.pos)
      }

      parentConstraint.foreach { cst =>
        trace(name, cst)
      }

      val constraintsFromConstrs = cases.foldLeft(List.empty[KindConstraint]) {
        case (cstsSoFar, nextConstr) =>
          cstsSoFar ++ gather(nextConstr, env)
      }

      val allConstraints = parentConstraint ++ constraintsFromConstrs

      Right(allConstraints)
  }

  def chainSubstitutions(ss: List[Substitution]): Substitution =
    ss.foldLeft(EmptySubst)(chainSubstitution)

  def chainSubstitutions(ss: Substitution*): Substitution =
    chainSubstitutions(ss.toList)

  def chainSubstitution(s1: Substitution, s2: Substitution): Substitution =
    s2 ++ s1.view.map { case (kindVar, kind) =>  (kindVar, kind.substitute(s2)) }.toMap

  def bind(kindVar: KindVariable, kind: Kind, pos: Pos): Infer[Substitution] =
    kind match {
      case k @ KindVariable(_) if kindVar == k =>
        Right(EmptySubst)
      case k if kindVar.occursIn(k) =>
        TypeError.kindOccursCheck(pos, kindVar, kind)
      case _ =>
        Right(Map(kindVar -> kind))
    }

  def unify(left: Kind, right: Kind, pos: Pos): Infer[Substitution] = {

    if (isTraceEnabled) {
      val lStr = Printer.print(left)
      val rStr = Printer.print(right)
      val llYellow = lStr.style(Style.Ansi.Fg.Yellow)
      val rrYellow = rStr.style(Style.Ansi.Fg.Yellow)
      val traceMsg = Doc.hardLine + Doc.text("Unifying") & llYellow & Doc.text("with") & rrYellow
      logger.info(traceMsg.render(context.consoleWidth))
    }

    def go(left: Kind, right: Kind): Infer[Substitution] = {
      (left, right) match {
        case (Parameterized(lParams, _), Parameterized(rParams, _)) if lParams.length != rParams.length =>
          TypeError.kindUnification(pos, left, right)

        case (Parameterized(lArgs, lRes), Parameterized(rArgs, rRes)) =>
          lArgs.zip(rArgs).foldM(EmptySubst) {
            case (subst, (lArg, rArg)) =>
              unify(lArg.substitute(subst), rArg.substitute(subst), pos).map { newSubst =>
                chainSubstitution(subst, newSubst)
              }
          }.flatMap { paramSubst =>
            unify(lRes.substitute(paramSubst), rRes.substitute(paramSubst), pos).map { resultSubst =>
              chainSubstitution(paramSubst, resultSubst)
            }
          }

        case (Atomic, Atomic) =>
          Right(EmptySubst)

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

  def kindcheck(data: Data[Meta.Typed], env: Environment[Meta.Typed]): Infer[(Data[Meta.Typed], Environment[Meta.Typed])] = {

    if (isTraceEnabled) {
      trace("Initial kind environment", env.kinds)
    }

    for {
      csts <- gather(data, env)

      subst <- solve(csts)

      _ = if (subst.nonEmpty && isTraceEnabled) {
        val header = Doc.hardLine + Doc.text("Apply substitution:")
        val substMsg = header & Printer.print(subst)(Printer.print, Printer.print)
        logger.info(substMsg.render(context.consoleWidth))
      }

      checkedData = data.substituteKinds(subst).defaultKinds

      updatedEnv = checkedData.typeParams
        .map(tv => tv.name -> tv.kind)
        .toMap.updated(checkedData.name, checkedData.kind)

      _ = if (updatedEnv.nonEmpty && isTraceEnabled) {
        trace("Final kind environment", updatedEnv)
      }

    } yield (checkedData, env.withKinds(updatedEnv))
  }
}
