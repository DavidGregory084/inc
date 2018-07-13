package inc.typechecker

import inc.common.Error

case class TypeError(private val message: String) extends Error(message)

object TypeError {
  def singleton(msg: String): Either[List[TypeError], Nothing] =
    Left(List(TypeError(msg)))
}
