package inc.parser

import inc.common.Error

case class ParserError(pos: Int, private val message: String) extends Error(message)

object ParserError {
  def singleton(idx: Int, msg: String): Either[List[ParserError], Nothing] =
    Left(List(ParserError(idx, msg)))
}
