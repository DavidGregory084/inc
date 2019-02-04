package inc.common

import java.lang.{ String, Throwable }
import scala.{ Product, Serializable }

abstract class Error(val pos: Pos, val msg: String) extends Throwable(msg) with Product with Serializable