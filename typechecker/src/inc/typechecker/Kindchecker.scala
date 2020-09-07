package inc.typechecker

import inc.common._
import cats.syntax.foldable._
import cats.syntax.flatMap._
import cats.syntax.monoid._
import scala.{ ::, Left, Right, Nil }
import scala.collection.immutable.{ List, Map }
import scala.Predef.ArrowAssoc
import com.typesafe.scalalogging.LazyLogging

object Kindchecker extends LazyLogging {
  type Subst = Substitution[KindVariable, Kind]

  def gather(typ: TypeExpr[Meta.Typed], env: Environment[Meta.Typed]): List[KindConstraint] =
    typ match {
      case TypeApplyExpr(typ, args, meta) =>
        val typKind = typ.meta.typ.typ.kind
        val resultKind = meta.typ.typ.kind

        val tpCsts = gather(typ, env)
        val tparamCsts = args.flatMap(gather(_, env))

        val tparamKinds = args.map(_.meta.typ.typ.kind)
        val appliedKind = Parameterized(tparamKinds, resultKind)
        val appCst = List(EqualKind(typKind, appliedKind, meta.pos))

        tpCsts ++ tparamCsts ++ appCst

      case TypeConstructorExpr(_, name, meta) =>
        val typKind = meta.typ.typ.kind
        val envKind = env.kinds.getOrElse(name, typKind)

        if (typKind != envKind)
          List(EqualKind(typKind, envKind, meta.pos))
        else
          List.empty
    }

  def gather(constr: DataConstructor[Meta.Typed], env: Environment[Meta.Typed]): List[KindConstraint] =
    constr match {
      case DataConstructor(_, params, _) =>
        params.foldLeft(List.empty[KindConstraint]) {
          case (cstsSoFar, Param(_, ascribedAs, meta)) =>
            val paramType = meta.typ.typ
            val paramKind = paramType.kind
            val paramResultCst = EqualKind(paramKind, Atomic, meta.pos)
            val paramCsts = gather(ascribedAs.get, env)
            cstsSoFar ++ List(paramResultCst) ++ paramCsts
        }
    }

  def gather(data: Data[Meta.Typed], env: Environment[Meta.Typed]): Infer[List[KindConstraint]] =
    data match {
      case Data(_, tparams, cases, meta) =>
        val dataKind = data.kind

        val inferredKind =
          if (tparams.isEmpty) {
            val TypeConstructor(_, tyConKind) = meta.typ.typ
            tyConKind
          } else {
            val TypeApply(TypeConstructor(_, tyConKind), _, _) = meta.typ.typ
            tyConKind
          }

        val parentConstraint =
          if (inferredKind == dataKind)
            List.empty
          else
            List(EqualKind(inferredKind, dataKind, meta.pos))

        val constraintsFromConstrs = cases.foldLeft(List.empty[KindConstraint]) {
          case (cstsSoFar, nextConstr) =>
            cstsSoFar ++ gather(nextConstr, env)
        }

        Right(parentConstraint ++ constraintsFromConstrs)
    }

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
