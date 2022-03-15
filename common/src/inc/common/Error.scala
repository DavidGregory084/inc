package inc.common

import java.lang.String
import java.lang.Throwable
import scala.Predef.wrapRefArray
import scala.Product
import scala.Serializable
import scala.StringContext

abstract class Error(
  val pos: Pos,
  val underlying: Throwable = null
) extends Throwable(underlying)
  with Product
  with Serializable {
  def message: String

  override def toString = {
    productPrefix + "(" + pos + "," + message + ")"
  }
}

object Error {
  def formatMessage(file: sourcecode.File, line: sourcecode.Line, msg: String): String = {
    val fileName = file.value.split("/").dropWhile(_ != "src").drop(1).mkString("/")
    s"${fileName}:${line.value} - $msg"
  }
}
