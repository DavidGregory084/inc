package inc.typechecker

import inc.common.{ Error, Pos }

case class TypeError(private val position: Pos, private val message: String) extends Error(position, message)

object TypeError {
  def singleton(pos: Pos, msg: String): Either[List[TypeError], Nothing] =
    Left(List(TypeError(pos, msg)))
}
