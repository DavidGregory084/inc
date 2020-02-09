package inc.parser

import inc.common.{ Error, Pos }
import java.lang.String
import scala.{ Either, Left, Nothing }
import scala.collection.immutable.List

case class ParserError(private val position: Pos, private val message: String) extends Error(position, message)

object ParserError {
  def singleton(msg: String): Either[List[ParserError], Nothing] = {
    Left(List(ParserError(Pos.Empty, msg)))
  }
}
