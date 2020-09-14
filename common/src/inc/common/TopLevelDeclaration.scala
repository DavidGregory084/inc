package inc.common

import cats.Functor
import cats.syntax.functor._
import java.lang.{ Exception, String }
import scala.Some
import scala.=:=
import scala.collection.immutable.{ List, Map }

sealed abstract class TopLevelDeclaration[A] extends SyntaxTree[A] {
  def name: String

  def meta: A

  def members: List[A]

  def toProto(implicit eqv: A =:= Meta.Typed): proto.TopLevelDeclaration = this match {
    case Let(name, expr, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Let(name, expr.toProto, nameWithType)
    case Data(name, tparams, cases, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Data(
        name,
        tparams.map(_.toProto),
        cases.map(_.toProto),
        nameWithType)
  }

  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed): TopLevelDeclaration[A] =
    if (subst.isEmpty)
      this
    else {
      val from = to.flip
      val typedMeta = from(meta.substitute(subst))
      this match {
        case data @ Data(_, _, _, _) =>
          data
        case let @ Let(_, binding, _) =>
          let.copy(
            binding = binding.substitute(subst),
            meta = typedMeta)
      }
    }

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= Meta.Typed): TopLevelDeclaration[A] = {
    val from = to.flip
    val typedMeta = from(meta.substituteKinds(subst))
    this match {
      case data @ Data(_, _, _, _) =>
        data.copy(
          typeParams = data.typeParams.map(_.substituteKinds(subst)),
          cases = data.cases.map(_.substituteKinds(subst)),
          meta = typedMeta)
      case let @ Let(_, binding, _) =>
        let.copy(
          binding = binding.substituteKinds(subst),
          meta = typedMeta)
    }
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): TopLevelDeclaration[A] = {
    val from = to.flip
    val typedMeta = from(meta.defaultKinds)
    this match {
      case data @ Data(_, _, _, _) =>
        data.copy(
          typeParams = data.typeParams.map(_.defaultKinds),
          cases = data.cases.map(_.defaultKinds),
          meta = typedMeta)
      case let @ Let(_, binding, _) =>
        let.copy(
          binding = binding.defaultKinds,
          meta = typedMeta)
    }
  }
}

object TopLevelDeclaration {
  def fromProto(decl: proto.TopLevelDeclaration): TopLevelDeclaration[Meta.Typed] = decl match {
    case let @ proto.Let(name, binding, _, _) =>
      Let(
        name,
        Expr.fromProto(binding),
        Meta.fromProto(let.getNameWithType))
    case data @ proto.Data(name, tparams, cases, _, _) =>
      Data(
        name,
        tparams.map(TypeConstructorExpr.fromProto).toList,
        cases.map(DataConstructor.fromProto).toList,
        Meta.fromProto(data.getNameWithType))
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
          typeParams = data.typeParams.map(_.map(f)),
          cases = data.cases.map(c => c.copy(params = c.params.map(_.map(f)), meta = f(c.meta))),
          meta = f(data.meta))
    }
  }

  implicit val topLevelDeclarationSubstitutableTypes: Substitutable[TypeVariable, Type, TopLevelDeclaration[Meta.Typed]] =
    new Substitutable[TypeVariable, Type, TopLevelDeclaration[Meta.Typed]] {
      def substitute(decl: TopLevelDeclaration[Meta.Typed], subst: Substitution[TypeVariable, Type]): TopLevelDeclaration[Meta.Typed] =
        decl.substitute(subst.subst)
    }

  implicit val topLevelDeclarationSubstitutableKinds: Substitutable[KindVariable, Kind, TopLevelDeclaration[Meta.Typed]] =
    new Substitutable[KindVariable, Kind, TopLevelDeclaration[Meta.Typed]] {
      def substitute(decl: TopLevelDeclaration[Meta.Typed], subst: Substitution[KindVariable, Kind]): TopLevelDeclaration[Meta.Typed] =
        decl.substituteKinds(subst.subst)
    }
}

final case class DataConstructor[A](
  name: String,
  params: List[Param[A]],
  meta: A
) extends SyntaxTree[A] {
  def toProto(implicit eqv: A =:= Meta.Typed): proto.DataConstructor = {
    val nameWithType = Some(eqv(meta).toProto)
    proto.DataConstructor(name, params.map(_.toProto), nameWithType)
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): DataConstructor[A] = {
    val from = to.flip
    val namePosType = to(meta)
    copy(
      params = params.map(_.defaultKinds),
      meta = from(namePosType.defaultKinds))
  }

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= Meta.Typed): DataConstructor[A] = {
    val from = to.flip
    val namePosType = to(meta)
    copy(
      params = params.map(_.substituteKinds(subst)),
      meta = from(namePosType.substituteKinds(subst)))
  }
}

object DataConstructor {
  def fromProto(data: proto.DataConstructor): DataConstructor[Meta.Typed] = data match {
    case data @ proto.DataConstructor(name, params, _, _) =>
      DataConstructor(
        name,
        params.map(Param.fromProto).toList,
        Meta.fromProto(data.getNameWithType))
  }
}

final case class Data[A](
  name: String,
  typeParams: List[TypeConstructorExpr[A]],
  cases: List[DataConstructor[A]],
  meta: A
) extends TopLevelDeclaration[A] {

  def kind(implicit eqv: A =:= Meta.Typed): Kind =
    if (typeParams.isEmpty)
      Atomic
    else
      Parameterized(typeParams.map(_.meta.typ.typ.kind), Atomic)

  def members = meta :: cases.map(_.meta)
}

final case class Let[A](
  name: String,
  binding: Expr[A],
  meta: A
) extends TopLevelDeclaration[A] {
  def members = List(meta)
}
