package inc.codegen

import inc.common.Error

case class CodegenError(private val message: String) extends Error(message)
