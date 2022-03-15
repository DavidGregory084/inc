package inc.codegen

import inc.common.Error
import inc.common.Pos

import java.lang.String
import java.lang.Throwable
import scala.Either
import scala.Left
import scala.Nothing
import scala.collection.immutable.List

case class CodegenError(val message: String, cause: Throwable = null)
  extends Error(Pos.Empty, cause)

object CodegenError {
  def singleton(
    msg: String
  )(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[CodegenError], Nothing] = {
    Left(List(CodegenError(Error.formatMessage(file, line, msg))))
  }
}
