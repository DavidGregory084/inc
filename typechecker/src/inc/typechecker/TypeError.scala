package inc.typechecker

import inc.common.{ Error, Pos }
import java.lang.String
import scala.{ Either, Left, Nothing }
import scala.collection.immutable.List

case class TypeError(private val position: Pos, private val message: String) extends Error(position, message)

object TypeError {
  def singleton(pos: Pos, msg: String): Either[List[TypeError], Nothing] =
    Left(List(TypeError(pos, msg)))
}
