package inc.common

import cats.Functor
import java.lang.{ Exception, String }
import scala.{ Option, Some }
import scala.=:=
import scala.collection.immutable.Map

final case class Param[A](name: String, ascribedAs: Option[TypeScheme], meta: A) {
  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= NamePosType) = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip
      Param(name, ascribedAs, from(to(meta).substitute(subst)))
    }
  }

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= NamePosType): Param[A] = {
    val from = to.flip
    val namePosType = to(meta)
    copy(
      ascribedAs = ascribedAs.map(_.substituteKinds(subst)),
      meta = from(namePosType.substituteKinds(subst)))
  }

  def withAscribedType(implicit eqv: A =:= NameWithPos): Param[NamePosType] =
    ascribedAs.map(asc => copy(meta = meta.withType(asc)))
      .getOrElse(throw new Exception("Called withAscribedType on a param with no ascription"))

  def toProto(implicit eqv: A =:= NamePosType): proto.Param = {
    val nameWithType = Some(eqv(meta).toProto)
    proto.Param(name, ascribedAs.map(_.toProto), nameWithType)
  }
}

object Param {
  def fromProto(param: proto.Param): Param[NameWithType] =
    Param(param.name, param.ascribedAs.map(TypeScheme.fromProto), NameWithType.fromProto(param.getNameWithType))

  implicit val paramFunctor: Functor[Param] = new Functor[Param] {
    def map[A, B](pa: Param[A])(f: A => B): Param[B] =
      pa.copy(meta = f(pa.meta))
  }
}
