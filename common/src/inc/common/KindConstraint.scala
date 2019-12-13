package inc.common

import scala.{ Product, Serializable }
import scala.collection.immutable.Map

sealed trait KindConstraint extends Product with Serializable {
  def pos: Pos
  def substitute(subst: Map[KindVariable, Kind]): KindConstraint = this match {
    case EqualKind(l, r, pos) => EqualKind(l.substitute(subst), r.substitute(subst), pos)
  }
}

case class EqualKind(l: Kind, r: Kind, pos: Pos) extends KindConstraint
