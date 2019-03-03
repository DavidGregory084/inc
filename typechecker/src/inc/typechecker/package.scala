package inc

import inc.common._

import cats.data.Chain
import java.lang.String
import scala.{ Either, Right }
import scala.collection.immutable.{ List, Map }

package object typechecker {
  type Infer[A] = Either[List[TypeError], A]

  type Environment = Map[String, TypeScheme]
  type Substitution = Map[TypeVariable, Type]

  val EmptyEnv: Environment = Map.empty
  val EmptySubst: Substitution = Map.empty
  val EmptyResult: Infer[(Chain[Expr[NamePosType]], Substitution)] = Right((Chain.empty, EmptySubst))

  def chainSubstitutions(ss: List[Substitution]): Substitution =
    ss.foldLeft(EmptySubst)(chainSubstitution)

  def chainSubstitutions(ss: Substitution*): Substitution =
    chainSubstitutions(ss.toList)

  def substitute(env: Environment, subst: Substitution): Environment = {
    if (subst.nonEmpty) scribe.trace(NL + "Apply substitution: " + Printer.print(subst))
    env.mapValues(_.substitute(subst))
  }

  def chainSubstitution(s1: Substitution, s2: Substitution): Substitution =
    s2 ++ s1.mapValues(_.substitute(s2))
}
