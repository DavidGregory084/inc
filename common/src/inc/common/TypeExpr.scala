package inc.common

import cats.Functor
import java.io.Serializable
import java.lang.String
import scala.{ =:=, Product }
import scala.collection.immutable.{ List, Map }
import scala.Predef.???

sealed abstract class TypeExpr[A] extends Product with Serializable {
  def meta: A

  def toType: Type = ???

  def toProto(implicit eqv: A =:= Meta.Typed): proto.TypeExpr = ???

  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed): TypeExpr[A] = ???

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= Meta.Typed): TypeExpr[A] = ???

  def defaultKinds(implicit eqv: A =:= Meta.Typed): TypeExpr[A] = ???
}

object TypeExpr {
  def fromProto(expr: proto.TypeExpr): TypeExpr[Meta.Typed] = ???

  implicit val typeExprFunctor: Functor[TypeExpr] = new Functor[TypeExpr] {
    def map[A, B](expr: TypeExpr[A])(f: A => B): TypeExpr[B] = ???
  }
}

case class TypeConstructorExpr[A](mod: List[String], name: String, meta: A) extends TypeExpr[A] {
  def fullName = if (mod.isEmpty) name else mod.mkString("/") + "." + name
  override def toProto(implicit eqv: A =:= Meta.Typed): inc.common.proto.TypeConstructorExpr = ???
  override def substitute(subst: Map[TypeVariable,Type])(implicit to: A =:= Meta.Typed): TypeConstructorExpr[A] = ???
  override def substituteKinds(subst: Map[KindVariable,Kind])(implicit to: A =:= Meta.Typed): TypeConstructorExpr[A] = ???
  override def defaultKinds(implicit eqv: A =:= Meta.Typed): TypeConstructorExpr[A] = ???
}

object TypeConstructorExpr {
  def fromProto(expr: proto.TypeConstructorExpr): TypeConstructorExpr[Meta.Typed] = ???

  implicit val typeConstructorExprFunctor: Functor[TypeConstructorExpr] = new Functor[TypeConstructorExpr] {
    def map[A, B](expr: TypeConstructorExpr[A])(f: A => B): TypeConstructorExpr[B] = ???
  }
}

case class TypeApplyExpr[A](typ: TypeExpr[A], args: List[TypeExpr[A]], meta: A) extends TypeExpr[A]
