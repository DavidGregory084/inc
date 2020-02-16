package inc.common

import scala.{ Product, Serializable, Some }
import scala.collection.immutable.Map

sealed abstract class Meta extends Product with Serializable {
  def pos: Pos
}

object Meta {
  case class Untyped(name: Name, pos: Pos) extends Meta {
    def forgetPos = copy(pos = Pos.Empty)

    def withType(typ: TypeScheme): Typed =
      Typed(name, typ, pos)

    def withSimpleType(typ: Type): Typed =
      withType(TypeScheme(typ))
  }

  case class Typed(name: Name, typ: TypeScheme, pos: Pos) extends Meta {
    def toProto = proto.NameWithType(
      name = name.toProto,
      `type` = Some(typ.toProto)
    )

    def forgetPos: Typed =
      copy(pos = Pos.Empty)

    def forgetType =
      Meta.Untyped(name, pos)

    def substitute(subst: Map[TypeVariable, Type]): Typed =
      if (subst.isEmpty)
        this
      else
        copy(typ = typ.substitute(subst))

    def substituteKinds(subst: Map[KindVariable, Kind]): Typed =
      copy(typ = typ.substituteKinds(subst))

    def defaultKinds: Typed =
      copy(typ = typ.defaultKinds)
  }

  def fromProto(nameWithType: proto.NameWithType): Typed =
    Typed(
      Name.fromProto(nameWithType.name),
      TypeScheme.fromProto(nameWithType.getType),
      Pos.Empty
    )
}
