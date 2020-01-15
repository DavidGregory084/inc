package inc.common

import java.lang.String
import scala.Boolean
import scala.collection.Iterable
import scala.collection.immutable.Map
import scala.Predef.ArrowAssoc

case class Environment(
  declarations: Map[String, Meta.Typed],
  types: Map[String, Kind]
) {
  def withDeclaration(name: String, meta: Meta.Typed) =
    copy(declarations = declarations.updated(name, meta))
  def withDeclarations(decls: Iterable[(String, Meta.Typed)]) =
    copy(declarations = declarations ++ decls)

  def withType(name: String, kind: Kind) =
    copy(types = types.updated(name, kind))
  def withTypes(tps: Iterable[(String, Kind)]) =
    copy(types = types ++ tps)

  def prefixed(mod: String) = {
    Environment(
      declarations.map { case (nm, meta) => (mod + "." + nm) -> meta },
      types
    )
  }

  def ++(env: Environment) = {
    Environment(
      declarations ++ env.declarations,
      types ++ env.types
    )
  }

  def filter(pred: String => Boolean): Environment = {
    Environment(
      declarations.view.filterKeys(pred).toMap,
      types.view.filterKeys(pred).toMap
    )
  }
}

object Environment {
  def empty = Environment(Map.empty, Map.empty)
}
