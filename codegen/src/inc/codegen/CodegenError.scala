package inc.codegen

import inc.common.{ Error, Pos }
import java.lang.{ String, Throwable }
import scala.{ Either, Left, Nothing, StringContext }
import scala.collection.immutable.List
import scala.Predef.wrapRefArray

case class CodegenError(private val message: String, cause: Throwable = null) extends Error(Pos.Empty, message, cause)

object CodegenError {
  def singleton(msg: String)(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[CodegenError], Nothing] = {
    val fileName = file.value.split("/").dropWhile(_ != "src").drop(1).mkString("/")
    Left(List(CodegenError(s"${fileName}:${line.value} - " + msg)))
  }
}
