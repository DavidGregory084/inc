package inc.common

import java.lang.String
import scala.{ Boolean, =:= }
import scala.collection.Iterable
import scala.collection.immutable.{List, Map}
import scala.Predef.ArrowAssoc

case class Environment[A](
  names: Map[String, Name] = Map.empty,
  types: Map[String, TypeScheme] = Map.empty,
  members: Map[Name, List[A]] = Map.empty[Name, List[A]],
  kinds: Map[String, Kind] = Map.empty
) {
  def withName(name: String, fullName: Name) =
    copy(names = names.updated(name, fullName))
  def withNames(nms: Iterable[(String, Name)]) =
    copy(names = names ++ nms)

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
      names.map { case (nm, fullName) => (prefix + "." + nm) -> fullName },
      types.map { case (nm, typ) => (prefix + "." + nm) -> typ.prefixed(prefix) },
      members.map { case (nm, mbrs) =>
        nm -> mbrs.map { m =>
          val typedMeta = eqv(m)
          typedMeta.copy(
            typ = typedMeta.typ.prefixed(prefix))
        }
      },
      kinds.map { case (nm, kind) => (prefix + "." + nm) -> kind }
    )
  }

  def ++(env: Environment[A]) = {
    Environment(
      names ++ env.names,
      types ++ env.types,
      members ++ env.members,
      kinds ++ env.kinds
    )
  }

  def filter(pred: String => Boolean): Environment[A] = {
    Environment(
      names.view.filterKeys(pred).toMap,
      types.view.filterKeys(pred).toMap,
      members,
      kinds.view.filterKeys(pred).toMap,
    )
  }

  def forgetMemberTypes(implicit eqv: A =:= Meta.Typed): Environment[Meta.Untyped] = {
    val untypedMembers = members.view.mapValues { _.map(_.forgetType) }.toMap
    Environment(names, types, untypedMembers, kinds)
  }
}

object Environment {
  def empty[A] = Environment[A]()
}
