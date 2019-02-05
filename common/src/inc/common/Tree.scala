package inc.common

import scala.{ Product, Serializable }

trait Tree extends Product with Serializable {
  def pos: Pos
}