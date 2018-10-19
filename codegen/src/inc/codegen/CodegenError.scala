package inc.codegen

import inc.common.{ Error, Pos }
import java.lang.String
import scala.{ Either, Left, Nothing }
import scala.collection.immutable.List

case class CodegenError(private val message: String) extends Error(Pos.Empty, message)

object CodegenError {
  def singleton(msg: String): Either[List[CodegenError], Nothing] =
    Left(List(CodegenError(msg)))
}
