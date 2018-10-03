package inc.parser

import inc.common.{ Error, Pos }

case class ParserError(private val position: Pos, private val message: String) extends Error(position, message)

object ParserError {
  def singleton(idx: Int, msg: String): Either[List[ParserError], Nothing] =
    Left(List(ParserError(Pos(idx, -1), msg)))
}
