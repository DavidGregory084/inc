package inc.common

import java.lang.{ Exception, String }
import java.util.concurrent.atomic.AtomicInteger
import scala.{ Option, Some, None, Int, Product, Serializable }
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

  def prefixed(prefix: String): Type = this match {
    case tyVar @ NamedTypeVariable(_, _) =>
      tyVar
    case tyVar @ InferredTypeVariable(_, _) =>
      tyVar
    case tyCon @ TypeConstructor(name, _)
        if Type.builtIns.contains(name) =>
      tyCon
    case tyCon @ TypeConstructor(name, _) =>
      tyCon.copy(name = (prefix + "." + name))
    case ta @ TypeApply(typ, params, _) =>
      ta.copy(
        typ = typ.prefixed(prefix),
        params = params.map(_.prefixed(prefix)))
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
    case tv @ NamedTypeVariable(_, _) =>
      tv.copy(kind = tv.kind.default)
    case tv @ InferredTypeVariable(_, _) =>
      tv.copy(kind = tv.kind.default)
    case tc @ TypeConstructor(_, _) =>
      tc.copy(kind = tc.kind.default)
    case ta @ TypeApply(_, _, _) =>
      val defaultedTyp = ta.typ.defaultKinds
      val defaultedTparams = ta.params.map(_.defaultKinds)
      ta.copy(
        typ = defaultedTyp,
        params = defaultedTparams,
        kind = ta.kind.default)
  }

  def substituteKinds(subst: Map[KindVariable, Kind]): Type = this match {
    case tv @ NamedTypeVariable(_, _) =>
      tv.copy(kind = kind.substitute(subst))
    case tv @ InferredTypeVariable(_, _) =>
      tv.copy(kind = kind.substitute(subst))
    case tc @ TypeConstructor(_, _) =>
      tc.copy(kind = kind.substitute(subst))
    case ta @ TypeApply(_, _, _) =>
      ta.copy(
        typ = ta.typ.substituteKinds(subst),
        params = ta.params.map(_.substituteKinds(subst)),
        kind = ta.kind.substitute(subst))
  }
}

object Type {
  val primitives = Set("Int", "Long", "Float", "Double", "Boolean", "Char")
  val builtIns = primitives ++ Set("->", "String", "Module", "Unit")

  val Int = TypeConstructor("Int", Atomic)
  val Long = TypeConstructor("Long", Atomic)
  val Float = TypeConstructor("Float", Atomic)
  val Double = TypeConstructor("Double", Atomic)
  val Boolean = TypeConstructor("Boolean", Atomic)
  val Char = TypeConstructor("Char", Atomic)
  val String = TypeConstructor("String", Atomic)
  val Module = TypeConstructor("Module", Atomic)
  val Unit = TypeConstructor("Unit", Atomic)

  val builtInTypes = Set(Int, Long, Float, Double, Boolean, Char, String, Module, Unit)

  def isFunction(typ: Type) = typ match {
    case Type.Function(_) =>
      true
    case _ =>
      false
  }

  object Function {
    def apply(from: List[Type], to: Type) = {
      val typeArgs = (from :+ to)
      val kindArgs = typeArgs.map(_ => Atomic)
      val funKind = Parameterized(kindArgs, Atomic)
      TypeApply(TypeConstructor("->", funKind), typeArgs, Atomic)
    }

    def unapply(typ: Type): Option[List[Type]] = typ match {
      case TypeApply(TypeConstructor("->", _), tpArgs, _) =>
        Some(tpArgs)
      case _ =>
        None
    }
  }

  def fromProto(typ: proto.Type): Type = typ match {
    case tyVar @ proto.TypeVariable(_, _) =>
      TypeVariable.fromProto(tyVar)
    case proto.TypeConstructor(name, kind, _) =>
      TypeConstructor(name, Kind.fromProto(kind))
    case proto.TypeApply(typ, params, kind, _) =>
      TypeApply(
        Type.fromProto(typ),
        params.map(Type.fromProto).toList,
        Kind.fromProto(kind))
    case proto.Type.Empty =>
      throw new Exception("Empty Type in protobuf")
  }

  implicit val typeSubstitutableTypes: Substitutable[TypeVariable, Type, Type] = new Substitutable[TypeVariable, Type, Type] {
    def substitute(typ: Type, subst: Substitution[TypeVariable, Type]): Type =
      typ.substitute(subst.subst)
  }

  implicit val typeSubstitutableKinds: Substitutable[KindVariable, Kind, Type] = new Substitutable[KindVariable, Kind, Type] {
    def substitute(typ: Type, subst: Substitution[KindVariable, Kind]): Type =
      typ.substituteKinds(subst.subst)
  }
}

sealed abstract class TypeVariable extends Type {
  def name: String

  def occursIn(typ: Type) =
    typ.freeTypeVariables.exists(_.name == this.name)

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
  def named(name: String, kind: Kind = KindVariable()) =
    NamedTypeVariable(name, kind)
  def apply(kind: Kind = Atomic): TypeVariable =
    InferredTypeVariable(nextId.getAndIncrement, kind)
  def fromProto(tyVar: proto.TypeVariable) = tyVar.tyVar match {
    case proto.TypeVariable.TyVar.Named(proto.NamedTypeVariable(name, kind, _)) =>
      NamedTypeVariable(name, Kind.fromProto(kind))
    case proto.TypeVariable.TyVar.Inferred(proto.InferredTypeVariable(id, kind, _)) =>
      InferredTypeVariable(id, Kind.fromProto(kind))
    case proto.TypeVariable.TyVar.Empty =>
      throw new Exception("Empty TypeVariable in protobuf")
  }
}

case class TypeConstructor(name: String, kind: Kind) extends Type

case class TypeApply(typ: Type, params: List[Type], kind: Kind) extends Type
