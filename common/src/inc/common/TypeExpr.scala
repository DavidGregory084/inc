package inc
package common

import cats.Functor
import cats.syntax.functor._
import java.lang.{ Exception, String }
import scala.{ =:=, Option, Some, None }
import scala.collection.immutable.{ List, Map }

sealed abstract class TypeExpr[A] extends SyntaxTree[A] {
  def meta: A

  def toProto(implicit eqv: A =:= Meta.Typed): proto.TypeExpr = this match {
    case TypeApplyExpr(typ, args, meta) =>
      proto.TypeApplyExpr(typ.toProto, args.map(_.toProto), Some(meta.toProto))
    case TypeConstructorExpr(name, meta) =>
      proto.TypeConstructorExpr(name, Some(meta.toProto))
  }

  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed): TypeExpr[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[TypeExpr]
      from(this.map(_.substitute(subst)))
    }
  }

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= Meta.Typed): TypeExpr[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[TypeExpr]
      from(this.map(_.substituteKinds(subst)))
    }
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): TypeExpr[A] = {
    val from = to.flip.liftCo[TypeExpr]
    from(this.map(_.defaultKinds))
  }
}

object TypeExpr {
  object Function {
    def unapply[A](typExpr: TypeExpr[A]): Option[List[TypeExpr[A]]] = typExpr match {
      case TypeApplyExpr(TypeConstructorExpr("->", _), tpArgs, _) =>
        Some(tpArgs)
      case _ =>
        None
    }
  }

  def fromProto(expr: proto.TypeExpr): TypeExpr[Meta.Typed] =
    expr match {
      case tyApp @ proto.TypeApplyExpr(typ, args, _, _) =>
        TypeApplyExpr(
          typ = TypeExpr.fromProto(typ),
          args = args.map(TypeExpr.fromProto).toList,
          meta = Meta.fromProto(tyApp.getNameWithType))
      case tyCon @ proto.TypeConstructorExpr(name, _, _) =>
        TypeConstructorExpr(name, Meta.fromProto(tyCon.getNameWithType))
      case proto.TypeExpr.Empty =>
        throw new Exception("Empty TypeExpr in protobuf")
    }

  implicit val typeExprFunctor: Functor[TypeExpr] = new Functor[TypeExpr] {
    def map[A, B](expr: TypeExpr[A])(f: A => B): TypeExpr[B] = expr match {
      case TypeApplyExpr(typ, args, meta) =>
        TypeApplyExpr(
         typ = map(typ)(f),
         args = args.map(map(_)(f)),
         meta = f(meta))
      case tyCon @ TypeConstructorExpr(_, meta) =>
        tyCon.copy(meta = f(meta))
    }
  }
}

case class TypeConstructorExpr[A](name: String, meta: A) extends TypeExpr[A] {
  override def toProto(implicit eqv: A =:= Meta.Typed): inc.common.proto.TypeConstructorExpr =
    proto.TypeConstructorExpr(name, Some(meta.toProto))
  override def substitute(subst: Map[TypeVariable,Type])(implicit to: A =:= Meta.Typed): TypeConstructorExpr[A] =
    to.flip.liftCo[TypeConstructorExpr](this.map(_.substitute(subst)))
  override def substituteKinds(subst: Map[KindVariable,Kind])(implicit to: A =:= Meta.Typed): TypeConstructorExpr[A] =
    to.flip.liftCo[TypeConstructorExpr](this.map(_.substituteKinds(subst)))
  override def defaultKinds(implicit to: A =:= Meta.Typed): TypeConstructorExpr[A] =
    to.flip.liftCo[TypeConstructorExpr](this.map(_.defaultKinds))
}

object TypeConstructorExpr {
  def fromProto(expr: proto.TypeConstructorExpr): TypeConstructorExpr[Meta.Typed] =
    TypeConstructorExpr(expr.name, Meta.fromProto(expr.getNameWithType))

  implicit val typeConstructorExprFunctor: Functor[TypeConstructorExpr] = new Functor[TypeConstructorExpr] {
    def map[A, B](expr: TypeConstructorExpr[A])(f: A => B): TypeConstructorExpr[B] =
      expr.copy(meta = f(expr.meta))
  }
}

case class TypeApplyExpr[A](typ: TypeExpr[A], args: List[TypeExpr[A]], meta: A) extends TypeExpr[A]
