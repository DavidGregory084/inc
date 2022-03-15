package inc.common

import cats.kernel.Monoid

import scala.=:=
import scala.AnyVal
import scala.Predef.ArrowAssoc
import scala.collection.immutable.List
import scala.collection.immutable.Map

case class Substitution[A, B](subst: Map[A, B]) extends AnyVal {
  def defaultKinds(implicit eqvA: A =:= TypeVariable, eqvB: B =:= Type): Substitution[A, B] = {
    Substitution(subst.map { case (tv, t) =>
      eqvA.flip(tv.defaultKinds) -> eqvB.flip(t.defaultKinds)
    })
  }
}

object Substitution {
  def empty[A, B]: Substitution[A, B] =
    Substitution(Map.empty)

  def chain[A, B](s1: Substitution[A, B], s2: Substitution[A, B])(implicit
    S: Substitutable[A, B, B]
  ): Substitution[A, B] =
    Substitution(s2.subst ++ s1.subst.view.map { case (a, b) => (a, S.substitute(b, s2)) }.toMap)

  def chain[A, B: Substitutable[A, B, *]](ss: List[Substitution[A, B]]): Substitution[A, B] =
    ss.foldLeft(Substitution.empty[A, B])(chain(_, _))

  def chain[A, B: Substitutable[A, B, *]](ss: Substitution[A, B]*): Substitution[A, B] =
    chain(ss.toList)

  implicit def substitutionMonoid[A, B: Substitutable[A, B, *]] = new Monoid[Substitution[A, B]] {
    def combine(x: Substitution[A, B], y: Substitution[A, B]): Substitution[A, B] = chain(x, y)
    def empty: Substitution[A, B] = Substitution.empty[A, B]
  }
}
