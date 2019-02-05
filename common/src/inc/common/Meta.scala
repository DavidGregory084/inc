package inc.common

import scala.Some
import scala.collection.immutable.Map

case class NameWithType(name: Name, typ: TypeScheme) {
  def substitute(subst: Map[TypeVariable, Type]): NameWithType =
    copy(typ = typ.substitute(subst))

  def toProto = proto.NameWithType(
    name = name.toProto,
    `type` = Some(typ.toProto)
  )
}

object NameWithType {
  def fromProto(nameWithType: proto.NameWithType): NameWithType =
    NameWithType(
      Name.fromProto(nameWithType.name),
      TypeScheme.fromProto(nameWithType.getType)
    )
}
