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
      this.map(a => from(to(a).substitute(subst)))
    }
}

object TopLevelDeclaration {
  def fromProto(decl: proto.TopLevelDeclaration): TopLevelDeclaration[Meta.Typed] = decl match {
    case let @ proto.Let(name, binding, _) =>
      Let(
        name,
        Expr.fromProto(binding),
        Meta.fromProto(let.getNameWithType))
    case data @ proto.Data(name, tparams, cases, _) =>
      Data(
        name,
        tparams.map(TypeVariable.fromProto).toList,
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
  def withAscribedTypes(tparams: List[TypeVariable])(implicit eqv: A =:= Meta.Untyped): DataConstructor[Meta.Typed] = {
    val checkedParams = params.map(_.withAscribedType)
    val typeScheme = TypeScheme(tparams, Type.Function(checkedParams.map(_.meta.typ.typ), returnType.typ))
    copy(params = checkedParams, meta = eqv(meta).withType(typeScheme))
  }

  def toProto(implicit eqv: A =:= Meta.Typed): proto.DataConstructor = {
    val nameWithType = Some(eqv(meta).toProto)
    proto.DataConstructor(name, params.map(_.toProto), Some(returnType.toProto), nameWithType)
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): DataConstructor[A] = {
    val from = to.flip
    val namePosType = to(meta)
    copy(
      params = params.map(_.defaultKinds),
      returnType = returnType.defaultKinds,
      meta = from(namePosType.defaultKinds))
  }

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= Meta.Typed): DataConstructor[A] = {
    val from = to.flip
    val namePosType = to(meta)
    copy(
      params = params.map(_.substituteKinds(subst)),
      returnType = returnType.substituteKinds(subst),
      meta = from(namePosType.substituteKinds(subst)))
  }
}

object DataConstructor {
  def fromProto(data: proto.DataConstructor): DataConstructor[Meta.Typed] = data match {
    case data @ proto.DataConstructor(name, params, _, _) =>
      DataConstructor(
        name,
        params.map(Param.fromProto).toList,
        TypeScheme.fromProto(data.getReturnType),
        Meta.fromProto(data.getNameWithType))
  }
}

final case class Data[A](
  name: String,
  typeParams: List[TypeVariable],
  cases: List[DataConstructor[A]],
  meta: A
) extends TopLevelDeclaration[A] {

  def kind: Kind =
    if (typeParams.isEmpty)
      Atomic
    else
      Parameterized(typeParams.map(_.kind), Atomic)

  def withAscribedTypes(implicit eqv: A =:= Meta.Untyped): Data[Meta.Typed] = {
    val checkedCases =
      cases.map(_.withAscribedTypes(typeParams))

    val tyCon =
      TypeConstructor(name, KindVariable(), meta.pos)

    val typeScheme =
      if (typeParams.isEmpty)
        TypeScheme(List.empty, tyCon)
      else
        TypeScheme(
          typeParams,
          TypeApply(tyCon, typeParams, KindVariable())
        )

    copy(
      cases = checkedCases,
      meta = meta.withType(typeScheme))
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): Data[A] = {
    val from = to.flip
    val namePosType = to(meta)
    copy(
      typeParams = typeParams.map(_.defaultKinds.asInstanceOf[TypeVariable]),
      cases = cases.map(_.defaultKinds),
      meta = from(namePosType.defaultKinds))
  }

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= Meta.Typed): Data[A] = {
    val from = to.flip
    val namePosType = to(meta)
    copy(
      typeParams = typeParams.map(_.substituteKinds(subst).asInstanceOf[TypeVariable]),
      cases = cases.map(_.substituteKinds(subst)),
      meta = from(namePosType.substituteKinds(subst)))
  }
}

final case class Let[A](
  name: String,
  binding: Expr[A],
  meta: A
) extends TopLevelDeclaration[A]
