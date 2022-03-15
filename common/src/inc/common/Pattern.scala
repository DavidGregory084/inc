package inc.common

import cats.Functor
import cats.syntax.functor._
import io.bullet.borer.Codec
import io.bullet.borer.derivation.ArrayBasedCodecs._

import java.lang.String
import scala.=:=
import scala.Boolean
import scala.Option
import scala.Predef.augmentString
import scala.collection.immutable.List
import scala.collection.immutable.Map
import scala.collection.immutable.Set

sealed abstract class Pattern[A] extends SyntaxTree[A] {
  def meta: A

  def isIrrefutable: Boolean = this match {
    case IdentPattern(_, _) =>
      true
    case ConstrPattern(_, _, _, _) =>
      false
  }

  def boundVariables(implicit eqv: A =:= Meta.Typed): Set[Name] = this match {
    case IdentPattern(_, meta) =>
      Set(meta.name)
    case ConstrPattern(_, _, patterns, meta) =>
      (Set(meta.name) ++ patterns.flatMap(_.boundVariables)).filterNot(_ == NoName)
  }

  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed): Pattern[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[Pattern]
      from(this.map(_.substitute(subst)))
    }
  }

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= Meta.Typed): Pattern[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[Pattern]
      from(this.map(_.substituteKinds(subst)))
    }
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): Pattern[A] = {
    val from = to.flip.liftCo[Pattern]
    from(this.map(_.defaultKinds))
  }
}

case class IdentPattern[A](name: String, meta: A) extends Pattern[A]

case class FieldPattern[A](name: String, pattern: Option[Pattern[A]], meta: A)
  extends SyntaxTree[A] {
  def boundVariables(implicit eqv: A =:= Meta.Typed): Set[Name] =
    pattern.toList.flatMap(_.boundVariables).toSet

  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed): FieldPattern[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[FieldPattern]
      from(this.map(_.substitute(subst)))
    }
  }

  def substituteKinds(
    subst: Map[KindVariable, Kind]
  )(implicit to: A =:= Meta.Typed): FieldPattern[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[FieldPattern]
      from(this.map(_.substituteKinds(subst)))
    }
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): FieldPattern[A] = {
    val from = to.flip.liftCo[FieldPattern]
    from(this.map(_.defaultKinds))
  }
}

object FieldPattern {
  implicit val fieldPatternFunctor: Functor[FieldPattern] = new Functor[FieldPattern] {
    def map[A, B](pa: FieldPattern[A])(f: A => B): FieldPattern[B] = pa match {
      case FieldPattern(field, pattern, meta) =>
        FieldPattern(field, pattern.map(_.map(f)), f(meta))
    }
  }

  implicit val fieldPatternCodec: Codec[FieldPattern[Meta.Typed]] =
    deriveCodec[FieldPattern[Meta.Typed]]
}

case class ConstrPattern[A](
  name: String,
  alias: Option[String],
  patterns: List[FieldPattern[A]],
  meta: A
) extends Pattern[A]

object Pattern {
  implicit val patternFunctor: Functor[Pattern] = new Functor[Pattern] {
    def map[A, B](pa: Pattern[A])(f: A => B): Pattern[B] = pa match {
      case ident @ IdentPattern(_, _) =>
        ident.copy(meta = f(ident.meta))
      case constr @ ConstrPattern(_, _, _, _) =>
        constr.copy(patterns = constr.patterns.map(_.map(f)), meta = f(constr.meta))
    }
  }

  implicit val patternCodec: Codec[Pattern[Meta.Typed]] = deriveAllCodecs[Pattern[Meta.Typed]]
}
