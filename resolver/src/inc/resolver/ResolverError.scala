package inc.resolver

import inc.common.{ Error, Pos }
import java.lang.String
import scala.{ Either, Left, Nothing }
import scala.collection.immutable.List

case class ResolverError(private val position: Pos, private val message: String) extends Error(position, message)

object ResolverError {
  def singleton(pos: Pos, msg: String): Either[List[ResolverError], Nothing] =
    Left(List(ResolverError(pos, msg)))
}
