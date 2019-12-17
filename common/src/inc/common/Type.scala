package inc.common

import java.lang.{ Exception, String }
import java.util.concurrent.atomic.AtomicInteger
import scala.{ Int, Product, Serializable }
import scala.collection.immutable.{ List, Map, Set }

sealed abstract class Type extends Product with Serializable {
  def kind: Kind

  def toProto: proto.Type = this match {
    case NamedTypeVariable(n, kind) =>
      proto.TypeVariable(
        proto.TypeVariable.TyVar.Named(
          proto.NamedTypeVariable(n, kind.toProto)))
    case InferredTypeVariable(i, kind) =>
      proto.TypeVariable(
        proto.TypeVariable.TyVar.Inferred(
          proto.InferredTypeVariable(i, kind.toProto)))
    case TypeApply(typ, params, kind) =>
      proto.TypeApply(typ.toProto, params.map(_.toProto), kind.toProto)
    case TypeConstructor(name, kind) =>
      proto.TypeConstructor(name, kind.toProto)
  }

  def isPrimitive = this match {
    case TypeConstructor(name, _) =>
      Type.primitives.contains(name)
    case _ =>
      false
  }

  def freeTypeVariables: Set[TypeVariable] = this match {
    case tyVar @ NamedTypeVariable(_, _) =>
      Set(tyVar)
    case tyVar @ InferredTypeVariable(_, _) =>
      Set(tyVar)
    case TypeApply(tyVar @ InferredTypeVariable(_, _), params, _) =>
      params.flatMap(_.freeTypeVariables).toSet + tyVar
    case TypeApply(tyVar @ NamedTypeVariable(_, _), params, _) =>
      params.flatMap(_.freeTypeVariables).toSet + tyVar
    case TypeApply(typ, params, _) =>
      typ.freeTypeVariables ++ params.flatMap(_.freeTypeVariables).toSet
    case TypeConstructor(_, _) =>
      Set.empty
  }

  def substitute(subst: Map[TypeVariable, Type]): Type =
    if (subst.isEmpty)
      this
    else this match {
      case tyVar @ NamedTypeVariable(_, _) =>
        subst.getOrElse(tyVar, tyVar)
      case tyVar @ InferredTypeVariable(_, _) =>
        subst.getOrElse(tyVar, tyVar)
      case tyCon @ TypeConstructor(_, _) =>
        tyCon
      case ta @ TypeApply(typ, params, _) =>
        ta.copy(
          typ = typ.substitute(subst),
          params = params.map(_.substitute(subst)))
    }

  def defaultKinds: Type = this match {
    case NamedTypeVariable(name, kind) =>
      NamedTypeVariable(name, kind.default)
    case InferredTypeVariable(id, kind) =>
      InferredTypeVariable(id, kind.default)
    case TypeConstructor(name, kind) =>
      TypeConstructor(name, kind.default)
    case TypeApply(typ, tparams, _) =>
      val defaultedTyp = typ.defaultKinds
      val defaultedTparams = tparams.map(_.defaultKinds)
      val resultKind = defaultedTyp.kind
      TypeApply(
        defaultedTyp,
        defaultedTparams,
        resultKind)
  }

  def substituteKinds(subst: Map[KindVariable, Kind]): Type = this match {
    case NamedTypeVariable(name, kind) =>
      NamedTypeVariable(name, kind.substitute(subst))
    case InferredTypeVariable(id, kind) =>
      InferredTypeVariable(id, kind.substitute(subst))
    case TypeConstructor(name, kind) =>
      TypeConstructor(name, kind.substitute(subst))
    case TypeApply(typ, tparams, kind) =>
      TypeApply(
        typ.substituteKinds(subst),
        tparams.map(_.substituteKinds(subst)),
        kind.substitute(subst))
  }
}

object Type {
  val UnitClass = "inc.rts.Unit"

  val primitives = Set("Int", "Long", "Float", "Double", "Boolean", "Char")

  val Int = TypeConstructor("Int", Atomic)
  val Long = TypeConstructor("Long", Atomic)
  val Float = TypeConstructor("Float", Atomic)
  val Double = TypeConstructor("Double", Atomic)
  val Boolean = TypeConstructor("Boolean", Atomic)
  val Char = TypeConstructor("Char", Atomic)
  val String = TypeConstructor("String", Atomic)
  val Module = TypeConstructor("Module", Atomic)
  val Unit = TypeConstructor(UnitClass, Atomic)
  def Function(from: List[Type], to: Type) = {
    TypeApply(
      TypeConstructor("->", Parameterized(from.map(_ => Atomic), Atomic)),
      from :+ to,
      Atomic)
  }

  def fromProto(typ: proto.Type): Type = typ match {
    case tyVar @ proto.TypeVariable(_) =>
      TypeVariable.fromProto(tyVar)
    case proto.TypeConstructor(name, kind) =>
      TypeConstructor(name, Kind.fromProto(kind))
    case proto.TypeApply(typ, params, kind) =>
      TypeApply(
        Type.fromProto(typ),
        params.map(Type.fromProto).toList,
        Kind.fromProto(kind))
    case proto.Type.Empty =>
      throw new Exception("Empty Type in protobuf")
  }
}

sealed abstract class TypeVariable extends Type {
  def name: String

  def occursIn(typ: Type) = typ.freeTypeVariables.contains(this)

  override def toProto: proto.TypeVariable = this match {
    case NamedTypeVariable(n, kind) =>
      proto.TypeVariable(
        proto.TypeVariable.TyVar.Named(
          proto.NamedTypeVariable(n, kind.toProto)))
    case InferredTypeVariable(i, kind) =>
      proto.TypeVariable(
        proto.TypeVariable.TyVar.Inferred(
          proto.InferredTypeVariable(i, kind.toProto)))
  }
}

case class NamedTypeVariable(name: String, kind: Kind) extends TypeVariable

case class InferredTypeVariable(id: Int, kind: Kind) extends TypeVariable {
  def name = "T" + id.toString
}

object TypeVariable {
  val nextId = new AtomicInteger(1)
  def apply(kind: Kind = Atomic): TypeVariable =
    InferredTypeVariable(nextId.getAndIncrement, kind)
  def fromProto(tyVar: proto.TypeVariable) = tyVar.tyVar match {
    case proto.TypeVariable.TyVar.Named(proto.NamedTypeVariable(name, kind)) =>
      NamedTypeVariable(name, Kind.fromProto(kind))
    case proto.TypeVariable.TyVar.Inferred(proto.InferredTypeVariable(id, kind)) =>
      InferredTypeVariable(id, Kind.fromProto(kind))
    case proto.TypeVariable.TyVar.Empty =>
      throw new Exception("Empty TypeVariable in protobuf")
  }
}

case class TypeConstructor(name: String, kind: Kind) extends Type

case class TypeApply(typ: Type, params: List[Type], kind: Kind) extends Type
