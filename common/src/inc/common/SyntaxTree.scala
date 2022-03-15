package inc.common

import java.io.Serializable
import scala.=:=
import scala.Product
import scala.collection.immutable.Map

abstract class SyntaxTree[A] extends Product with Serializable {
  def meta: A
  def substitute(subst: Map[TypeVariable, Type])(implicit eqv: A =:= Meta.Typed): SyntaxTree[A]
  def substituteKinds(subst: Map[KindVariable, Kind])(implicit eqv: A =:= Meta.Typed): SyntaxTree[A]
  def defaultKinds(implicit eqv: A =:= Meta.Typed): SyntaxTree[A]
}
