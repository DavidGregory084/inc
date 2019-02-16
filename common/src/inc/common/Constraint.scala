package inc.common

import scala.{ Product, Serializable }

sealed trait Constraint extends Product with Serializable

case object True extends Constraint

case object False extends Constraint

case class And(l: Constraint, r: Constraint) extends Constraint

case class Equal(l: Type, r: Type) extends Constraint
