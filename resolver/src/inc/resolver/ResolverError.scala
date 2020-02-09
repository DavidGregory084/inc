package inc.resolver

import inc.common.{ Error, Pos }
import java.lang.String
import scala.{ Either, Left, Nothing, StringContext }
import scala.collection.immutable.List
import scala.Predef.wrapRefArray

case class ResolverError(private val position: Pos, private val message: String) extends Error(position, message)

object ResolverError {
  def singleton(pos: Pos, msg: String)(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[ResolverError], Nothing] = {
    val fileName = file.value.split("/").dropWhile(_ != "src").drop(1).mkString("/")
    Left(List(ResolverError(pos, s"${fileName}:${line.value} - " + msg)))
  }
}
