package inc.common

import scala.{ Product, Serializable }

sealed trait Constraint extends Product with Serializable {
  def pos: Pos
}

case class Equal(l: Type, r: Type, pos: Pos) extends Constraint
