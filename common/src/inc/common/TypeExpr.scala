package inc
package common

import cats.Functor
import cats.syntax.functor._
import io.bullet.borer._
import io.bullet.borer.derivation.ArrayBasedCodecs._
import java.lang.String
import scala.{ =:=, Option, Some, None }
import scala.collection.immutable.{ List, Map }
import scala.Predef.augmentString

sealed abstract class TypeExpr[A] extends SyntaxTree[A] {
  def meta: A

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

  implicit val typeExprCodec: Codec[TypeExpr[Meta.Typed]] = deriveAllCodecs[TypeExpr[Meta.Typed]]
}

case class TypeConstructorExpr[A](name: String, meta: A) extends TypeExpr[A] {
  override def substitute(subst: Map[TypeVariable,Type])(implicit to: A =:= Meta.Typed): TypeConstructorExpr[A] =
    to.flip.liftCo[TypeConstructorExpr](this.map(_.substitute(subst)))
  override def substituteKinds(subst: Map[KindVariable,Kind])(implicit to: A =:= Meta.Typed): TypeConstructorExpr[A] =
    to.flip.liftCo[TypeConstructorExpr](this.map(_.substituteKinds(subst)))
  override def defaultKinds(implicit to: A =:= Meta.Typed): TypeConstructorExpr[A] =
    to.flip.liftCo[TypeConstructorExpr](this.map(_.defaultKinds))
}

object TypeConstructorExpr {
  implicit val typeConstructorExprFunctor: Functor[TypeConstructorExpr] = new Functor[TypeConstructorExpr] {
    def map[A, B](expr: TypeConstructorExpr[A])(f: A => B): TypeConstructorExpr[B] =
      expr.copy(meta = f(expr.meta))
  }

  implicit val typeConstructorExprCodec: Codec[TypeConstructorExpr[Meta.Typed]] = deriveCodec[TypeConstructorExpr[Meta.Typed]]
}

case class TypeApplyExpr[A](typ: TypeExpr[A], args: List[TypeExpr[A]], meta: A) extends TypeExpr[A]
