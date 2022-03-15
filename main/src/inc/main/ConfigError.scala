package inc.main

import inc.common.Error
import inc.common.Pos

import java.lang.String
import scala.StringContext
import scala.collection.immutable.List

case class ConfigError(private val position: Pos, val message: String) extends Error(position)

object ConfigError {
  def invalidClasspathEntry(entry: String): List[ConfigError] =
    List(ConfigError(Pos.Empty, s"Error while parsing classpath entry: $entry"))

  def missingClassData(pos: Pos, className: String): List[ConfigError] =
    List(ConfigError(pos, s"The class ${className} was not found on the classpath"))
}
