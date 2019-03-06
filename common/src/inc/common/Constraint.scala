package inc.common

import scala.{ Product, Serializable }
import scala.collection.immutable.Map

sealed trait Constraint extends Product with Serializable {
  def pos: Pos
  def substitute(subst: Map[TypeVariable, Type]): Constraint = this match {
    case Equal(l, r, pos) => Equal(l.substitute(subst), r.substitute(subst), pos)
  }
}

case class Equal(l: Type, r: Type, pos: Pos) extends Constraint
