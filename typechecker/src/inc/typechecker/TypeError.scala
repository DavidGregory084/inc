package inc.typechecker

import inc.common.{ Error, Pos }
import java.lang.String
import scala.{ Either, Left, Nothing, StringContext }
import scala.collection.immutable.List
import scala.Predef.wrapRefArray

case class TypeError(private val position: Pos, private val message: String) extends Error(position, message)

object TypeError {
  def singleton(pos: Pos, msg: String)(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[TypeError], Nothing] = {
    val fileName = file.value.split("/").dropWhile(_ != "src").drop(1).mkString("/")
    Left(List(TypeError(pos, s"${fileName}:${line.value} - " + msg)))
  }
}
