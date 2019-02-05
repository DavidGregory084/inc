package inc.common

import cats.Functor
import cats.syntax.functor._
import java.lang.{ Exception, String }
import scala.Some
import scala.Predef.=:=
import scala.collection.immutable.Map

sealed abstract class TopLevelDeclaration[A] extends Tree {
  def name: String

  def meta: A

  def toProto(implicit eqv: A =:= NameWithType): proto.TopLevelDeclaration = this match {
    case Let(name, expr, _, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Let(name, expr.toProto, nameWithType)
  }

  def substitute(subst: Map[TypeVariable, Type])(implicit eqv: A =:= NameWithType): TopLevelDeclaration[A] =
    this.map(a => eqv(a).substitute(subst).asInstanceOf[A])
}

object TopLevelDeclaration {
  def fromProto(decl: proto.TopLevelDeclaration): TopLevelDeclaration[NameWithType] = decl match {
    case let @ proto.Let(name, binding, _) =>
      Let(
        name,
        Expr.fromProto(binding),
        Pos.Empty,
        NameWithType.fromProto(let.getNameWithType))
    case proto.TopLevelDeclaration.Empty =>
      throw new Exception("Empty TopLevelDeclaration in protobuf")
  }
  implicit val topLevelDeclarationFunctor: Functor[TopLevelDeclaration] = new Functor[TopLevelDeclaration] {
    def map[A, B](ta: TopLevelDeclaration[A])(f: A => B): TopLevelDeclaration[B] = ta match {
      case let @ Let(_, _, _, _) =>
        let.copy(
          binding = let.binding.map(f),
          meta = f(let.meta))
    }
  }
}

final case class Let[A](name: String, binding: Expr[A], pos: Pos, meta: A) extends TopLevelDeclaration[A]