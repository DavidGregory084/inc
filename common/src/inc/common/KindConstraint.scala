package inc.common

import scala.Product
import scala.Serializable
import scala.collection.immutable.Map

sealed abstract class KindConstraint extends Product with Serializable {
  def pos: Pos

  def substitute(subst: Map[KindVariable, Kind]): KindConstraint =
    if (subst.isEmpty)
      this
    else
      this match {
        case EqualKind(l, r, pos) =>
          EqualKind(l.substitute(subst), r.substitute(subst), pos)
      }
}

object KindConstraint {
  implicit val kindConstraintSubstitutableKinds: Substitutable[KindVariable, Kind, KindConstraint] =
    new Substitutable[KindVariable, Kind, KindConstraint] {
      def substitute(
        constr: KindConstraint,
        subst: Substitution[KindVariable, Kind]
      ): KindConstraint =
        constr.substitute(subst.subst)
    }
}

case class EqualKind(l: Kind, r: Kind, pos: Pos) extends KindConstraint
