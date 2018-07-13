package inc.codegen

import inc.common.Error

case class CodegenError(private val message: String) extends Error(message)

object CodegenError {
  def singleton(msg: String): Either[List[CodegenError], Nothing] =
    Left(List(CodegenError(msg)))
}
