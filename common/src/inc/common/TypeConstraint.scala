package inc.common

import scala.{ Boolean, Product, Serializable }
import scala.collection.immutable.Map

sealed abstract class TypeConstraint extends Product with Serializable {
  def pos: Pos
;
  def substitute(subst: Map[TypeVariable, Type]): TypeConstraint =
    if (subst.isEmpty)
      this
    else this match {
      case EqualType(l, r, pos) =>
        EqualType(l.substitute(subst), r.substitute(subst), pos)
    }

  def substituteKinds(subst: Map[KindVariable, Kind]): TypeConstraint =
    if (subst.isEmpty)
      this
    else this match {
      case EqualType(l, r, pos) =>
        EqualType(l.substituteKinds(subst), r.substituteKinds(subst), pos)
    }

  def defaultKinds: TypeConstraint =
    this match {
      case EqualType(l, r, pos) =>
        EqualType(l.defaultKinds, r.defaultKinds, pos)
    }

  def containsError: Boolean =
    this match {
      case EqualType(l, r, _) =>
        l.containsError || r.containsError
    }
}

object TypeConstraint {
  implicit val constraintSubstitutableTypes: Substitutable[TypeVariable, Type, TypeConstraint] =
    new Substitutable[TypeVariable, Type, TypeConstraint] {
      def substitute(constr: TypeConstraint, subst: Substitution[TypeVariable, Type]): TypeConstraint =
        constr.substitute(subst.subst)
    }

  implicit val constraintSubstitutableKinds: Substitutable[KindVariable, Kind, TypeConstraint] =
    new Substitutable[KindVariable, Kind, TypeConstraint] {
      def substitute(constr: TypeConstraint, subst: Substitution[KindVariable, Kind]): TypeConstraint =
        constr.substituteKinds(subst.subst)
    }
}

case class EqualType(l: Type, r: Type, pos: Pos) extends TypeConstraint
