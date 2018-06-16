package inc.parser

import inc.common.Error

case class ParserError(pos: Int, private val message: String) extends Error(message)
