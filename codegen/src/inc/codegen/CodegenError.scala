package inc.codegen

import inc.common.{ Error, Pos }

case class CodegenError(private val message: String) extends Error(Pos.Empty, message)

object CodegenError {
  def singleton(msg: String): Either[List[CodegenError], Nothing] =
    Left(List(CodegenError(msg)))
}
