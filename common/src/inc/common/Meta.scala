package inc.common

import io.bullet.borer._
import io.bullet.borer.derivation.ArrayBasedCodecs._
import scala.{ Product, Serializable }
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

  object Typed {
    implicit val typedMetaSubstitutableTypes: Substitutable[TypeVariable, Type, Typed] = new Substitutable[TypeVariable, Type, Typed] {
      def substitute(meta: Typed, subst: Substitution[TypeVariable,Type]): Typed =
        meta.substitute(subst.subst)
    }
    implicit val typedMetaSubstitutableKinds: Substitutable[KindVariable, Kind, Typed] = new Substitutable[KindVariable, Kind, Typed] {
      def substitute(meta: Typed, subst: Substitution[KindVariable, Kind]): Typed =
        meta.substituteKinds(subst.subst)
    }

    implicit val typedCodec: Codec[Typed] = deriveCodec[Typed]
  }
}
