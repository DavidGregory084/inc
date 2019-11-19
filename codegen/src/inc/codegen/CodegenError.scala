package inc.codegen

import inc.common.{ Error, Pos }
import java.lang.{ String, Throwable }
import scala.{ Either, Left, Nothing }
import scala.collection.immutable.List

case class CodegenError(private val message: String, cause: Throwable = null) extends Error(Pos.Empty, message, cause)

object CodegenError {
  def singleton(msg: String): Either[List[CodegenError], Nothing] =
    Left(List(CodegenError(msg)))
}
