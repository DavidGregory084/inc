package inc.typechecker

import inc.common._
import cats.instances.either._
import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.flatMap._
import scala.{ ::, Left, Right, Nil }
import scala.collection.immutable.{ List, Map }
import scala.Predef.ArrowAssoc
import com.typesafe.scalalogging.LazyLogging

object Kindchecker extends LazyLogging {
  type Substitution = Map[KindVariable, Kind]
  val EmptySubst: Substitution = Map.empty

  def gather(typ: Type, env: Environment[Meta.Typed]): List[KindConstraint] = typ match {
    case NamedTypeVariable(_, _, _) =>
      List.empty

    case InferredTypeVariable(_, _, _) =>
      List.empty

    case TypeConstructor(nm, kind, pos) =>
      val envKind = env.kinds.getOrElse(nm, kind)

      if (kind != envKind)
        List(EqualKind(kind, envKind, pos))
      else
        List.empty

    case TypeApply(tp, appliedTps, kind, pos) =>

      val tpCsts = gather(tp, env)

      val tparamCsts = appliedTps.flatMap(gather(_, env))

      val tparamKinds = appliedTps.map(_.kind)
      val appliedKind = Parameterized(tparamKinds, kind)

      val appCst = List(EqualKind(tp.kind, appliedKind, pos))

      tpCsts ++ tparamCsts ++ appCst
  }

  def gather(constr: DataConstructor[Meta.Typed], env: Environment[Meta.Typed]): List[KindConstraint] = constr match {
    case DataConstructor(_, params, _, _) =>
      val constraints = params.foldLeft(List.empty[KindConstraint]) {
        case (cstsSoFar, Param(_, _, meta)) =>
          val paramResultCst = EqualKind(meta.typ.typ.kind, Atomic, meta.pos)
          val paramCsts = gather(meta.typ.typ, env)
          cstsSoFar ++ List(paramResultCst) ++ paramCsts
      }

      constraints
  }

  def gather(data: Data[Meta.Typed], env: Environment[Meta.Typed]): Infer[List[KindConstraint]] = data match {
    case Data(_, tparams, cases, meta) =>
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

  def kindcheck(data: Data[Meta.Typed], env: Environment[Meta.Typed]): Infer[(Environment[Meta.Typed], Substitution)] = {
    for {
      csts <- gather(data, env)

      subst <- solve(csts)

      checkedData @ Data(_, _, _, _) = data.substituteKinds(subst).defaultKinds

      updatedEnv = checkedData.typeParams
        .map(tv => tv.name -> tv.kind)
        .toMap.updated(checkedData.name, checkedData.kind)

    } yield (env.withKinds(updatedEnv), subst)
  }
}
