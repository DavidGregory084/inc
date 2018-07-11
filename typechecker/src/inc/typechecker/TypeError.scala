package inc.typechecker

import inc.common.Error

case class TypeError(private val message: String) extends Error(message)
