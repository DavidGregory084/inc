package inc.common

import scala.Some
import scala.collection.immutable.Map

case class NameWithPos(name: Name, pos: Pos) {
  def withType(typ: TypeScheme): NamePosType =
    NamePosType(name, pos, typ)
  def withSimpleType(typ: Type): NamePosType =
    withType(TypeScheme(typ))
}

case class NameWithType(name: Name, typ: TypeScheme) {
  def withEmptyPos: NamePosType =
    NamePosType(name, Pos.Empty, typ)

  def substitute(subst: Map[TypeVariable, Type]): NameWithType =
    if (subst.isEmpty)
      this
    else
      copy(typ = typ.substitute(subst))
}

object NameWithType {
  def fromProto(nameWithType: proto.NameWithType): NameWithType =
    NameWithType(
      Name.fromProto(nameWithType.name),
      TypeScheme.fromProto(nameWithType.getType)
    )
}

case class NamePosType(name: Name, pos: Pos, typ: TypeScheme) {
  def toProto = proto.NameWithType(
    name = name.toProto,
    `type` = Some(typ.toProto)
  )

  def withEmptyPos = copy(pos = Pos.Empty)

  def forgetPos = NameWithType(name, typ)

  def substitute(subst: Map[TypeVariable, Type]): NamePosType =
    if (subst.isEmpty)
      this
    else
      copy(typ = typ.substitute(subst))

  def substituteKinds(subst: Map[KindVariable, Kind]): NamePosType =
    copy(typ = typ.substituteKinds(subst))
}
