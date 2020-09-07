package inc.common

import cats.Functor
import java.io.Serializable
import java.lang.String
import scala.{ =:=, Product }
import scala.collection.immutable.{ List, Map }
import scala.Predef.???

sealed abstract class TypeExpr[A] extends Product with Serializable {
  def meta: A

  def toProto(implicit eqv: A =:= Meta.Typed): proto.TypeExpr = ???

  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed): TypeExpr[A] = {
    val from = to.flip
    if (subst.isEmpty)
      this
    else
      this match {
        case tyCon @ TypeConstructorExpr(_, _, meta) =>
          tyCon.copy(meta = from(meta.substitute(subst)))
        case tyApp @ TypeApplyExpr(typ, args, meta) =>
          tyApp.copy(
            typ = typ.substitute(subst),
            args.map(_.substitute(subst)),
            meta = from(meta.substitute(subst)))
      }
  }

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= Meta.Typed): TypeExpr[A] = {
    val from = to.flip
    if (subst.isEmpty)
      this
    else
      this match {
        case tyCon @ TypeConstructorExpr(_, _, meta) =>
          tyCon.copy(meta = from(meta.substituteKinds(subst)))
        case tyApp @ TypeApplyExpr(typ, args, meta) =>
          tyApp.copy(
            typ = typ.substituteKinds(subst),
            args = args.map(_.substituteKinds(subst)),
            meta = from(meta.substituteKinds(subst)))
      }
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): TypeExpr[A] = {
    val from = to.flip
    this match {
      case tyCon @ TypeConstructorExpr(_, _, meta) =>
        tyCon.copy(meta = from(meta.defaultKinds))
      case tyApp @ TypeApplyExpr(typ, args, meta) =>
        tyApp.copy(
          typ = typ.defaultKinds,
          args = args.map(_.defaultKinds),
          meta = from(meta.defaultKinds))
    }
  }
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
  override def substitute(subst: Map[TypeVariable,Type])(implicit to: A =:= Meta.Typed): TypeConstructorExpr[A] =
    this.copy(meta = to.flip(meta.substitute(subst)))
  override def substituteKinds(subst: Map[KindVariable,Kind])(implicit to: A =:= Meta.Typed): TypeConstructorExpr[A] =
    this.copy(meta = to.flip(meta.substituteKinds(subst)))
  override def defaultKinds(implicit to: A =:= Meta.Typed): TypeConstructorExpr[A] =
    this.copy(meta = to.flip(meta.defaultKinds))
}

object TypeConstructorExpr {
  def fromProto(expr: proto.TypeConstructorExpr): TypeConstructorExpr[Meta.Typed] = ???

  implicit val typeConstructorExprFunctor: Functor[TypeConstructorExpr] = new Functor[TypeConstructorExpr] {
    def map[A, B](expr: TypeConstructorExpr[A])(f: A => B): TypeConstructorExpr[B] = ???
  }
}

case class TypeApplyExpr[A](typ: TypeExpr[A], args: List[TypeExpr[A]], meta: A) extends TypeExpr[A]
