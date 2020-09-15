package inc.common

import cats.Functor
import cats.syntax.functor._
import java.lang.String
import scala.{ Option, Some, None }
import scala.=:=
import scala.collection.immutable.Map

final case class Param[A](
  name: String,
  ascribedAs: Option[TypeExpr[A]],
  meta: A
) extends SyntaxTree[A] {
  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed) = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip
      Param(name, ascribedAs, from(to(meta).substitute(subst)))
    }
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): Param[A] = {
    val from = to.flip
    val namePosType = to(meta)
    copy(
      ascribedAs = ascribedAs.map(_.defaultKinds),
      meta = from(namePosType.defaultKinds))
  }

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= Meta.Typed): Param[A] = {
    val from = to.flip
    val namePosType = to(meta)
    copy(
      ascribedAs = ascribedAs.map(_.substituteKinds(subst)),
      meta = from(namePosType.substituteKinds(subst)))
  }

  def toProto(implicit eqv: A =:= Meta.Typed): proto.Param = {
    val nameWithType = Some(eqv(meta).toProto)
    proto.Param(name, ascribedAs.map(_.toProto).getOrElse(proto.TypeExpr.Empty), nameWithType)
  }
}

object Param {
  def fromProto(param: proto.Param): Param[Meta.Typed] = param match {
    case proto.Param(name, proto.TypeExpr.Empty, _, _) =>
      Param(name, None, Meta.fromProto(param.getNameWithType))
    case proto.Param(name, ascribedAs, _, _) =>
      Param(name, Some(TypeExpr.fromProto(ascribedAs)), Meta.fromProto(param.getNameWithType))
  }

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
}
