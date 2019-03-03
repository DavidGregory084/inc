package inc.common

import cats.Functor
import cats.syntax.functor._
import java.lang.{ Exception, String }
import scala.{ Product, Serializable, Some }
import scala.Predef.=:=
import scala.collection.immutable.Map

sealed trait TopLevelDeclaration[A] extends Product with Serializable {
  def name: String

  def meta: A

  def toProto(implicit eqv: A =:= NamePosType): proto.TopLevelDeclaration = this match {
    case Let(name, expr, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Let(name, expr.toProto, nameWithType)
  }

  def substitute(subst: Map[TypeVariable, Type])(implicit eqv: A =:= NamePosType): TopLevelDeclaration[A] =
    this.map(a => eqv(a).substitute(subst).asInstanceOf[A])
}

object TopLevelDeclaration {
  def fromProto(decl: proto.TopLevelDeclaration): TopLevelDeclaration[NameWithType] = decl match {
    case let @ proto.Let(name, binding, _) =>
      Let(
        name,
        Expr.fromProto(binding),
        NameWithType.fromProto(let.getNameWithType))
    case proto.TopLevelDeclaration.Empty =>
      throw new Exception("Empty TopLevelDeclaration in protobuf")
  }
  implicit val topLevelDeclarationFunctor: Functor[TopLevelDeclaration] = new Functor[TopLevelDeclaration] {
    def map[A, B](ta: TopLevelDeclaration[A])(f: A => B): TopLevelDeclaration[B] = ta match {
      case let @ Let(_, _, _) =>
        let.copy(
          binding = let.binding.map(f),
          meta = f(let.meta))
    }
  }
}

final case class Let[A](
  name: String,
  binding: Expr[A],
  meta: A
) extends TopLevelDeclaration[A]
