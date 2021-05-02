package inc.common

import cats.Functor
import cats.syntax.functor._
import io.bullet.borer.Codec
import io.bullet.borer.derivation.ArrayBasedCodecs._
import java.lang.String
import scala.Option
import scala.=:=
import scala.collection.immutable.Map

final case class Param[A](
  name: String,
  ascribedAs: Option[TypeExpr[A]],
  meta: A
) extends SyntaxTree[A] {
  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed): Param[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[Param]
      from(this.map(_.substitute(subst)))
    }
  }

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= Meta.Typed): Param[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[Param]
      from(this.map(_.substituteKinds(subst)))
    }
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): Param[A] = {
    val from = to.flip.liftCo[Param]
    from(this.map(_.defaultKinds))
  }
}

object Param {
  implicit val paramFunctor: Functor[Param] = new Functor[Param] {
    def map[A, B](pa: Param[A])(f: A => B): Param[B] =
      pa.copy(
        ascribedAs = pa.ascribedAs.map(_.map(f)),
        meta = f(pa.meta))
  }

  implicit val paramSubstitutableTypes: Substitutable[TypeVariable, Type, Param[Meta.Typed]] = new Substitutable[TypeVariable, Type, Param[Meta.Typed]] {
    def substitute(param: Param[Meta.Typed], subst: Substitution[TypeVariable, Type]): Param[Meta.Typed] =
      param.substitute(subst.subst)
  }

  implicit val paramSubstitutableKinds: Substitutable[KindVariable, Kind, Param[Meta.Typed]] = new Substitutable[KindVariable, Kind, Param[Meta.Typed]] {
    def substitute(param: Param[Meta.Typed], subst: Substitution[KindVariable, Kind]): Param[Meta.Typed] =
      param.substituteKinds(subst.subst)
  }

  implicit val paramCodec: Codec[Param[Meta.Typed]] = deriveCodec[Param[Meta.Typed]]
}
