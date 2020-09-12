package inc.common

import java.lang.String
import scala.{ Boolean, =:= }
import scala.collection.Iterable
import scala.collection.immutable.{List, Map}
import scala.Predef.ArrowAssoc

case class Environment[A](
  valueNames: Map[String, Name] = Map.empty,
  typeNames: Map[String, Name] = Map.empty,
  types: Map[String, TypeScheme] = Map.empty,
  kinds: Map[String, Kind] = Map.empty,
  members: Map[Name, List[A]] = Map.empty[Name, List[A]],
) {
  def withValueName(name: String, fullName: Name) =
    copy(valueNames = valueNames.updated(name, fullName))
  def withValueNames(nms: Iterable[(String, Name)]) =
    copy(valueNames = valueNames ++ nms)

  def withTypeName(name: String, fullName: Name) =
    copy(typeNames = typeNames.updated(name, fullName))
  def withTypeNames(nms: Iterable[(String, Name)]) =
    copy(typeNames = typeNames ++ nms)

  def withType(name: String, typ: TypeScheme) =
    copy(types = types.updated(name, typ))
  def withTypes(tps: Iterable[(String, TypeScheme)]) =
    copy(types = types ++ tps)

  def withMember(name: Name, member: A) =
    copy(members = members.updated(name, member :: members.getOrElse(name, List.empty)))
  def withMembers(name: Name, mbrs: List[A]) =
    copy(members = members.updated(name, mbrs ++ members.getOrElse(name, List.empty)))

  def withKind(name: String, kind: Kind) =
    copy(kinds = kinds.updated(name, kind))
  def withKinds(knds: Iterable[(String, Kind)]) =
    copy(kinds = kinds ++ knds)

  def prefixed(prefix: String)(implicit eqv: A =:= Meta.Typed) = {
    Environment(
      valueNames.map { case (nm, fullName) => (prefix + "." + nm) -> fullName },
      typeNames.map { case (nm, fullName) => (prefix + "." + nm) -> fullName },
      types.map { case (nm, typ) => (prefix + "." + nm) -> typ.prefixed(prefix) },
      kinds.map { case (nm, kind) => (prefix + "." + nm) -> kind },
      members.map { case (nm, mbrs) =>
        nm -> mbrs.map { m =>
          val typedMeta = eqv(m)
          typedMeta.copy(
            typ = typedMeta.typ.prefixed(prefix))
        }
      },
    )
  }

  def ++(env: Environment[A]) = {
    Environment(
      valueNames ++ env.valueNames,
      typeNames ++ env.typeNames,
      types ++ env.types,
      kinds ++ env.kinds,
      members ++ env.members,
    )
  }

  def filter(pred: String => Boolean): Environment[A] = {
    Environment(
      valueNames.view.filterKeys(pred).toMap,
      typeNames.view.filterKeys(pred).toMap,
      types.view.filterKeys(pred).toMap,
      kinds.view.filterKeys(pred).toMap,
      members,
    )
  }

  def forgetMemberTypes(implicit eqv: A =:= Meta.Typed): Environment[Meta.Untyped] = {
    val untypedMembers = members.view.mapValues { _.map(_.forgetType) }.toMap
    Environment(valueNames, typeNames, types, kinds, untypedMembers)
  }

  def substituteTypes(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed): Environment[A] = {
    val from = to.flip
    copy(
      types = types.view.mapValues(_.substitute(subst)).toMap,
      members = members.view.mapValues(_.map(meta => from(meta.substitute(subst)))).toMap)
  }

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= Meta.Typed): Environment[A] = {
    val from = to.flip
    copy(
      types = types.view.mapValues(_.substituteKinds(subst)).toMap,
      members = members.view.mapValues(_.map(meta => from(meta.substituteKinds(subst)))).toMap)
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): Environment[A] = {
    val from = to.flip
    copy(
      types = types.view.mapValues(_.defaultKinds).toMap,
      members = members.view.mapValues(_.map(meta => from(meta.defaultKinds))).toMap)
  }
}

object Environment {
  def empty[A] = Environment[A](
    typeNames = Type.builtInTypes
      .map(tp => tp.name -> LocalName(tp.name))
      .toMap,
    types = Type.builtInTypes
      .map(tp => tp.name -> TypeScheme(tp))
      .toMap,
    kinds = Type.builtInTypes
      .map(tp => tp.name -> tp.kind)
      .toMap
  )
}
