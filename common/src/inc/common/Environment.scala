package inc.common

import java.lang.String
import scala.Boolean
import scala.collection.Iterable
import scala.collection.immutable.Map
import scala.Predef.ArrowAssoc

case class Environment(
  names: Map[String, Name] = Map.empty,
  types: Map[String, TypeScheme] = Map.empty,
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

  def withKind(name: String, kind: Kind) =
    copy(kinds = kinds.updated(name, kind))
  def withKinds(knds: Iterable[(String, Kind)]) =
    copy(kinds = kinds ++ knds)

  def prefixed(prefix: String) = {
    Environment(
      names.map { case (nm, fullName) => (prefix + "." + nm) -> fullName },
      types.map { case (nm, typ) => (prefix + "." + nm) -> typ },
      kinds
    )
  }

  def ++(env: Environment) = {
    Environment(
      names ++ env.names,
      types ++ env.types,
      kinds ++ env.kinds
    )
  }

  def filter(pred: String => Boolean): Environment = {
    Environment(
      names.view.filterKeys(pred).toMap,
      types.view.filterKeys(pred).toMap,
      kinds.view.filterKeys(pred).toMap,
    )
  }
}

object Environment {
  def empty = Environment()
}
