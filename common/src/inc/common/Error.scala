package inc.common

import java.lang.{ String, Throwable }
import scala.{ Product, Serializable }

abstract class Error(
  val pos: Pos,
  val msg: String,
  val underlying: Throwable = null
) extends Throwable(msg, underlying) with Product with Serializable
