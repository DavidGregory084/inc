package inc.common

import scala.Int

case class Pos(from: Int, to: Int) {
  def isEmpty = this == Pos.Empty
}

object Pos {
  def Empty = Pos(0, 0)
}
