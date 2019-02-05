package inc.common

import cats.Functor
import java.lang.String
import scala.Some
import scala.Predef.=:=
import scala.collection.immutable.Map

final case class Param[A](name: String, pos: Pos, meta: A) extends Tree {
  def substitute(subst: Map[TypeVariable, Type])(implicit eqv: A =:= NameWithType) =
    Param(name, pos, eqv(meta).substitute(subst).asInstanceOf[A])

  def toProto(implicit eqv: A =:= NameWithType): proto.Param = {
    val nameWithType = Some(eqv(meta).toProto)
    proto.Param(name, nameWithType)
  }
}

object Param {
  def fromProto(param: proto.Param): Param[NameWithType] =
    Param(param.name, Pos.Empty, NameWithType.fromProto(param.getNameWithType))

  implicit val paramFunctor: Functor[Param] = new Functor[Param] {
    def map[A, B](pa: Param[A])(f: A => B): Param[B] =
      pa.copy(meta = f(pa.meta))
  }
}
