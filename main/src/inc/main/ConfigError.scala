package inc.main

import inc.common.{ Error, Pos }
import java.lang.String
import scala.collection.immutable.List
import scala.StringContext

case class ConfigError(private val position: Pos, private val message: String) extends Error(position, message)

object ConfigError {
  def invalidClasspathEntry(entry: String): List[ConfigError] =
    List(ConfigError(Pos.Empty, s"Error while parsing classpath entry: $entry"))

  def missingClassData(pos: Pos, className: String): List[ConfigError] =
    List(ConfigError(pos, s"The class ${className} was not found on the classpath"))
}
