package inc.resolver

import inc.common.{ Error, Pos }
import java.lang.String
import scala.{ Either, Left, Nothing }
import scala.collection.immutable.List

case class ResolverError(private val position: Pos, val message: String) extends Error(position)

object ResolverError {
  def singleton(pos: Pos, msg: String)(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[ResolverError], Nothing] = {
    Left(List(ResolverError(pos, Error.formatMessage(file, line, msg))))
  }
}
