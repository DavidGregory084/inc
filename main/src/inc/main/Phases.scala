package inc.main

import java.lang.String
import scala.{ Product, Serializable }
import scala.collection.immutable.List

sealed abstract class Phase(val name: String) extends Product with Serializable

object Phase {
  case object Parser extends Phase("parser")
  case object Resolver extends Phase("resolver")
  case object Typer extends Phase("typer")
  case object Codegen extends Phase("codegen")
  val values = List(Parser, Resolver, Typer, Codegen)
  val names = values.map(_.name)
  def withName(name: String) = values.find(_.name == name)
}
