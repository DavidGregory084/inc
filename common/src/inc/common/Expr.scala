package inc.common

import cats.Functor
import cats.syntax.functor._
import io.bullet.borer.Codec
import io.bullet.borer.derivation.ArrayBasedCodecs._

import java.lang.String
import scala.=:=
import scala.Boolean
import scala.Char
import scala.Double
import scala.Float
import scala.Int
import scala.Long
import scala.Predef.augmentString
import scala.collection.immutable.List
import scala.collection.immutable.Map
import scala.collection.immutable.Set

sealed abstract class Expr[A] extends SyntaxTree[A] {
  def meta: A

  def capturedVariables(implicit eqv: A =:= Meta.Typed): Set[Reference[A]] = {
    val captured = this match {
      case LiteralInt(_, _)     => Set.empty
      case LiteralLong(_, _)    => Set.empty
      case LiteralFloat(_, _)   => Set.empty
      case LiteralDouble(_, _)  => Set.empty
      case LiteralBoolean(_, _) => Set.empty
      case LiteralString(_, _)  => Set.empty
      case LiteralChar(_, _)    => Set.empty
      case LiteralUnit(_)       => Set.empty
      case ref @ Reference(_, _, _) =>
        Set(ref)
      case If(cond, thenExpr, elseExpr, _) =>
        cond.capturedVariables ++
          thenExpr.capturedVariables ++
          elseExpr.capturedVariables
      case Lambda(params, body, _) =>
        val lambdaParams = params.map(_.meta.name)
        body.capturedVariables.filterNot { captured =>
          lambdaParams.contains(captured.meta.name)
        }
      case Apply(fn, args, _) =>
        fn.capturedVariables ++
          args.flatMap(_.capturedVariables)
      case Ascription(expr, _, _) =>
        expr.capturedVariables
      case Match(matchExpr, cases, _) =>
        matchExpr.capturedVariables ++ cases.flatMap { case MatchCase(pattern, resultExpr, _) =>
          resultExpr.capturedVariables.filterNot { captured =>
            pattern.boundVariables.contains(captured.meta.name)
          }
        }
    }

    captured.toList
      .distinctBy(_.name)
      .toSet
  }

  def replace(mapping: Map[Name, Reference[A]])(implicit to: A =:= Meta.Typed): Expr[A] =
    this match {
      case int @ LiteralInt(_, _)      => int
      case long @ LiteralLong(_, _)    => long
      case flt @ LiteralFloat(_, _)    => flt
      case dbl @ LiteralDouble(_, _)   => dbl
      case bool @ LiteralBoolean(_, _) => bool
      case str @ LiteralString(_, _)   => str
      case chr @ LiteralChar(_, _)     => chr
      case unit @ LiteralUnit(_)       => unit
      case ref @ Reference(_, _, _) =>
        mapping.getOrElse(
          meta.name,
          ref
        )
      case ifExpr @ If(cond, thenExpr, elseExpr, _) =>
        ifExpr.copy(
          cond = cond.replace(mapping),
          thenExpr = thenExpr.replace(mapping),
          elseExpr = elseExpr.replace(mapping)
        )
      case lam @ Lambda(_, body, _) =>
        lam.copy(body = body.replace(mapping))
      case app @ Apply(fn, args, _) =>
        app.copy(fn = fn.replace(mapping), args = args.map(_.replace(mapping)))
      case asc @ Ascription(expr, _, _) =>
        asc.copy(expr = expr.replace(mapping))
      case mat @ Match(matchExpr, cases, _) =>
        mat.copy(matchExpr = matchExpr.replace(mapping), cases = cases.map(_.replace(mapping)))
    }

  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed): Expr[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[Expr]
      from(this.map(_.substitute(subst)))
    }
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): Expr[A] = {
    val from = to.flip.liftCo[Expr]
    from(this.map(_.defaultKinds))
  }

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= Meta.Typed): Expr[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[Expr]
      from(this.map(_.substituteKinds(subst)))
    }
  }
}

object Expr {
  implicit val exprFunctor: Functor[Expr] = new Functor[Expr] {
    def map[A, B](ea: Expr[A])(f: A => B): Expr[B] = ea match {
      case int @ LiteralInt(_, _) =>
        int.copy(meta = f(int.meta))
      case long @ LiteralLong(_, _) =>
        long.copy(meta = f(long.meta))
      case float @ LiteralFloat(_, _) =>
        float.copy(meta = f(float.meta))
      case double @ LiteralDouble(_, _) =>
        double.copy(meta = f(double.meta))
      case boolean @ LiteralBoolean(_, _) =>
        boolean.copy(meta = f(boolean.meta))
      case string @ LiteralString(_, _) =>
        string.copy(meta = f(string.meta))
      case char @ LiteralChar(_, _) =>
        char.copy(meta = f(char.meta))
      case unit @ LiteralUnit(_) =>
        unit.copy(meta = f(unit.meta))
      case ref @ Reference(_, _, _) =>
        ref.copy(meta = f(ref.meta))
      case ifExpr @ If(_, _, _, _) =>
        ifExpr.copy(
          cond = map(ifExpr.cond)(f),
          thenExpr = map(ifExpr.thenExpr)(f),
          elseExpr = map(ifExpr.elseExpr)(f),
          meta = f(ifExpr.meta)
        )
      case lambda @ Lambda(_, _, _) =>
        lambda.copy(
          params = lambda.params.map(_.map(f)),
          body = map(lambda.body)(f),
          meta = f(lambda.meta)
        )
      case app @ Apply(_, _, _) =>
        app.copy(fn = map(app.fn)(f), args = app.args.map(map(_)(f)), meta = f(app.meta))
      case asc @ Ascription(_, _, _) =>
        asc.copy(expr = map(asc.expr)(f), ascribedAs = asc.ascribedAs.map(f), meta = f(asc.meta))
      case mat @ Match(_, _, _) =>
        mat.copy(
          matchExpr = map(mat.matchExpr)(f),
          cases = mat.cases.map(_.map(f)),
          meta = f(mat.meta)
        )
    }
  }

  implicit val exprSubstitutableTypes: Substitutable[TypeVariable, Type, Expr[Meta.Typed]] =
    new Substitutable[TypeVariable, Type, Expr[Meta.Typed]] {
      def substitute(
        expr: Expr[Meta.Typed],
        subst: Substitution[TypeVariable, Type]
      ): Expr[Meta.Typed] =
        expr.substitute(subst.subst)
    }
  implicit val exprSubstitutableKinds: Substitutable[KindVariable, Kind, Expr[Meta.Typed]] =
    new Substitutable[KindVariable, Kind, Expr[Meta.Typed]] {
      def substitute(
        expr: Expr[Meta.Typed],
        subst: Substitution[KindVariable, Kind]
      ): Expr[Meta.Typed] =
        expr.substituteKinds(subst.subst)
    }

  implicit val exprCodec: Codec[Expr[Meta.Typed]] = deriveAllCodecs[Expr[Meta.Typed]]
}

final case class If[A](
  cond: Expr[A],
  thenExpr: Expr[A],
  elseExpr: Expr[A],
  meta: A
) extends Expr[A]

final case class Lambda[A](
  params: List[Param[A]],
  body: Expr[A],
  meta: A
) extends Expr[A]

final case class Apply[A](
  fn: Expr[A],
  args: List[Expr[A]],
  meta: A
) extends Expr[A]

final case class Ascription[A](
  expr: Expr[A],
  ascribedAs: TypeExpr[A],
  meta: A
) extends Expr[A]

final case class MatchCase[A](
  pattern: Pattern[A],
  resultExpr: Expr[A],
  meta: A
) extends SyntaxTree[A] {
  def replace(mapping: Map[Name, Reference[A]])(implicit to: A =:= Meta.Typed): MatchCase[A] =
    copy(resultExpr = resultExpr.replace(mapping))

  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed): MatchCase[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[MatchCase]
      from(this.map(_.substitute(subst)))
    }
  }

  def substituteKinds(
    subst: Map[KindVariable, Kind]
  )(implicit to: A =:= Meta.Typed): MatchCase[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[MatchCase]
      from(this.map(_.substituteKinds(subst)))
    }
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): MatchCase[A] = {
    val from = to.flip.liftCo[MatchCase]
    from(this.map(_.defaultKinds))
  }
}

object MatchCase {
  implicit val matchCaseFunctor: Functor[MatchCase] = new Functor[MatchCase] {
    def map[A, B](ma: MatchCase[A])(f: A => B): MatchCase[B] = {
      ma.copy(pattern = ma.pattern.map(f), resultExpr = ma.resultExpr.map(f), meta = f(ma.meta))
    }
  }

  implicit val matchCaseCodec: Codec[MatchCase[Meta.Typed]] = deriveCodec[MatchCase[Meta.Typed]]
}

final case class Match[A](
  matchExpr: Expr[A],
  cases: List[MatchCase[A]],
  meta: A
) extends Expr[A] {
  def isExhaustive(env: Environment[Meta.Typed])(implicit eqv: A =:= Meta.Typed): Boolean = {
    cases.exists(_.pattern.isIrrefutable) ||
    env.members
      .get(matchExpr.meta.name)
      .map { dataMembers =>
        dataMembers.forall { dataMember =>
          cases.exists {
            case MatchCase(ConstrPattern(name, _, _, _), _, _) =>
              dataMember.name.shortName == name
            case _ =>
              false
          }
        }
      }
      .getOrElse(false)
  }
}

final case class LiteralInt[A](i: Int, meta: A)         extends Expr[A]
final case class LiteralLong[A](l: Long, meta: A)       extends Expr[A]
final case class LiteralFloat[A](f: Float, meta: A)     extends Expr[A]
final case class LiteralDouble[A](d: Double, meta: A)   extends Expr[A]
final case class LiteralBoolean[A](b: Boolean, meta: A) extends Expr[A]
final case class LiteralChar[A](c: Char, meta: A)       extends Expr[A]
final case class LiteralString[A](s: String, meta: A)   extends Expr[A]
final case class LiteralUnit[A](meta: A)                extends Expr[A]

final case class Reference[A](mod: List[String], name: String, meta: A) extends Expr[A] {
  def fullName = if (mod.isEmpty) name else mod.mkString("/") + "." + name
}
