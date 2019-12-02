package inc.common

import cats.Functor
import java.lang.String
import scala.{ Option, Some }
import scala.=:=
import scala.collection.immutable.Map

final case class Param[A](name: String, ascribedAs: Option[TypeScheme], meta: A) {
  def substitute(subst: Map[TypeVariable, Type])(implicit eqv: A =:= NamePosType) =
    Param(name, ascribedAs, eqv(meta).substitute(subst).asInstanceOf[A])

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
