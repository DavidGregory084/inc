package inc.common

import io.bullet.borer.Codec
import io.bullet.borer.derivation.ArrayBasedCodecs._
import java.lang.String
import java.util.concurrent.atomic.AtomicInteger
import scala.{ Boolean, Int, Option, Some, None, Product, Serializable, StringContext }
import scala.collection.immutable.{ List, Map, Set }
import scala.Predef.augmentString

sealed abstract class Type extends Product with Serializable {
  def kind: Kind

  def isPrimitive = this match {
    case TypeConstructor(name, _) =>
      Type.primitives.contains(name)
    case _ =>
      false
  }

  def toExpr: TypeExpr[Meta.Typed] = this match {
    case InferredTypeVariable(id, _) =>
      TypeConstructorExpr(
        name = s"T${id}",
        meta = Meta.Typed(LocalName(s"T${id}"), TypeScheme(this), Pos.Empty))
    case NamedTypeVariable(name, _) =>
      TypeConstructorExpr(
        name = name,
        meta = Meta.Typed(LocalName(name), TypeScheme(this), Pos.Empty))
    case TypeApply(typ, params, _) =>
      TypeApplyExpr(
        typ = typ.toExpr,
        args = params.map(_.toExpr),
        meta = Meta.Typed(NoName, TypeScheme(this), Pos.Empty))
    case TypeConstructor(name, _) =>
      TypeConstructorExpr(
        name = name,
        meta = Meta.Typed(LocalName(name), TypeScheme(this), Pos.Empty))
    case ErrorType =>
      TypeConstructorExpr(
        name = "<error>",
        meta = Meta.Typed(NoName, TypeScheme(this), Pos.Empty))
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
    case ErrorType =>
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
      case ErrorType =>
        ErrorType
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
    case ErrorType =>
      ErrorType
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
    case ErrorType =>
      ErrorType
  }

  def containsError: Boolean = this match {
    case InferredTypeVariable(_, _) =>
      false
    case NamedTypeVariable(_, _) =>
      false
    case TypeConstructor(_, _) =>
      false
    case TypeApply(ErrorType, _, _) =>
      true
    case TypeApply(_, args, _) =>
      args.exists(_.containsError)
    case ErrorType => true
  }
}

object Type {
  val primitives = Set("Int", "Long", "Float", "Double", "Boolean", "Char")

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
  val builtIns = builtInTypes.map(_.name) + "->"

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

  implicit val typeSubstitutableTypes: Substitutable[TypeVariable, Type, Type] = new Substitutable[TypeVariable, Type, Type] {
    def substitute(typ: Type, subst: Substitution[TypeVariable, Type]): Type =
      typ.substitute(subst.subst)
  }

  implicit val typeSubstitutableKinds: Substitutable[KindVariable, Kind, Type] = new Substitutable[KindVariable, Kind, Type] {
    def substitute(typ: Type, subst: Substitution[KindVariable, Kind]): Type =
      typ.substituteKinds(subst.subst)
  }

  implicit val typeCodec: Codec[Type] = deriveAllCodecs[Type]
}

sealed abstract class TypeVariable extends Type {
  def name: String

  def occursIn(typ: Type) =
    typ.freeTypeVariables.exists(_.name == this.name)

  override def substituteKinds(subst: Map[KindVariable, Kind]): TypeVariable = this match {
    case tv @ NamedTypeVariable(_, _) =>
      tv.copy(kind = kind.substitute(subst))
    case tv @ InferredTypeVariable(_, _) =>
      tv.copy(kind = kind.substitute(subst))
  }

  override def defaultKinds: TypeVariable = this match {
    case tv @ NamedTypeVariable(_, _) =>
      tv.copy(kind = kind.default)
    case tv @ InferredTypeVariable(_, _) =>
      tv.copy(kind = kind.default)
  }

  override def toExpr: TypeConstructorExpr[Meta.Typed] = this match {
    case InferredTypeVariable(id, _) =>
      TypeConstructorExpr(
        name = s"T${id}",
        meta = Meta.Typed(LocalName(s"T${id}"), TypeScheme(List.empty, this), Pos.Empty))
    case NamedTypeVariable(name, _) =>
      TypeConstructorExpr(
        name = name,
        meta = Meta.Typed(LocalName(name), TypeScheme(List.empty, this), Pos.Empty))
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
  def apply(kind: Kind = Atomic): InferredTypeVariable =
    InferredTypeVariable(nextId.getAndIncrement, kind)
  implicit val typeVariableCodec: Codec[TypeVariable] = deriveAllCodecs[TypeVariable]
}

case class TypeConstructor(name: String, kind: Kind) extends Type

case class TypeApply(typ: Type, params: List[Type], kind: Kind) extends Type

case object ErrorType extends Type {
  val kind = Atomic
}
