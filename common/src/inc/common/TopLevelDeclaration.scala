package inc.common

import cats.Functor
import cats.syntax.functor._
import java.lang.{ Exception, String }
import scala.{ Product, Serializable, Some }
import scala.=:=
import scala.collection.immutable.{ List, Map }

sealed abstract class TopLevelDeclaration[A] extends Product with Serializable {
  def name: String

  def meta: A

  def toProto(implicit eqv: A =:= NamePosType): proto.TopLevelDeclaration = this match {
    case Let(name, expr, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Let(name, expr.toProto, nameWithType)
    case Data(name, tparams, cases, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Data(
        name,
        tparams.map(t => proto.TypeVariable(t.id)),
        cases.map(_.toProto),
        nameWithType)
  }

  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= NamePosType): TopLevelDeclaration[A] =
    if (subst.isEmpty)
      this
    else {
      val from = to.flip
      this.map(a => from(to(a).substitute(subst)))
    }
}

object TopLevelDeclaration {
  def fromProto(decl: proto.TopLevelDeclaration): TopLevelDeclaration[NameWithType] = decl match {
    case let @ proto.Let(name, binding, _) =>
      Let(
        name,
        Expr.fromProto(binding),
        NameWithType.fromProto(let.getNameWithType))
    case data @ proto.Data(name, tparams, cases, _) =>
      Data(
        name,
        tparams.map(TypeVariable.fromProto).toList,
        cases.map(DataConstructor.fromProto).toList,
        NameWithType.fromProto(data.getNameWithType))
    case proto.TopLevelDeclaration.Empty =>
      throw new Exception("Empty TopLevelDeclaration in protobuf")
  }
  implicit val topLevelDeclarationFunctor: Functor[TopLevelDeclaration] = new Functor[TopLevelDeclaration] {
    def map[A, B](ta: TopLevelDeclaration[A])(f: A => B): TopLevelDeclaration[B] = ta match {
      case let @ Let(_, _, _) =>
        let.copy(
          binding = let.binding.map(f),
          meta = f(let.meta))
      case data @ Data(_, _, _, _) =>
        data.copy(
          cases = data.cases.map(c => c.copy(params = c.params.map(_.map(f)), meta = f(c.meta))),
          meta = f(data.meta))
    }
  }
}

final case class DataConstructor[A](
  name: String,
  params: List[Param[A]],
  returnType: TypeScheme,
  meta: A
) {
  def toProto(implicit eqv: A =:= NamePosType): proto.DataConstructor = {
    val nameWithType = Some(eqv(meta).toProto)
    proto.DataConstructor(name, params.map(_.toProto), Some(returnType.toProto), nameWithType)
  }
}

object DataConstructor {
  def fromProto(data: proto.DataConstructor): DataConstructor[NameWithType] = data match {
    case data @ proto.DataConstructor(name, params, _, _) =>
      DataConstructor(
        name,
        params.map(Param.fromProto).toList,
        TypeScheme.fromProto(data.getReturnType),
        NameWithType.fromProto(data.getNameWithType))
  }
}

final case class Data[A](
  name: String,
  typeParams: List[TypeVariable],
  cases: List[DataConstructor[A]],
  meta: A
) extends TopLevelDeclaration[A]

final case class Let[A](
  name: String,
  binding: Expr[A],
  meta: A
) extends TopLevelDeclaration[A]
