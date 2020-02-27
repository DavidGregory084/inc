package inc.common

import scala.{ Product, Serializable }
import scala.collection.immutable.Map

sealed abstract class Constraint extends Product with Serializable {
  def pos: Pos
  def substitute(subst: Map[TypeVariable, Type]): Constraint =
    if (subst.isEmpty)
      this
    else this match {
      case Equal(l, r, pos) => Equal(l.substitute(subst), r.substitute(subst), pos)
    }

  def substituteKinds(subst: Map[KindVariable, Kind]): Constraint =
    if (subst.isEmpty)
      this
    else this match {
      case Equal(l, r, pos) => Equal(l.substituteKinds(subst), r.substituteKinds(subst), pos)
    }

  def defaultKinds: Constraint =
    this match {
      case Equal(l, r, pos) =>
        Equal(l.defaultKinds, r.defaultKinds, pos)
    }
}

case class Equal(l: Type, r: Type, pos: Pos) extends Constraint
