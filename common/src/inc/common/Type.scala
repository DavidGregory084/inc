package inc.common

import java.lang.{ Exception, String }
import java.util.concurrent.atomic.AtomicInteger
import scala.{ Int, StringContext }
import scala.collection.immutable.{ List, Map, Set }

sealed trait Type {
  def kind: Kind

  def toProto: proto.Type = this match {
    case TypeVariable(i, kind) =>
      proto.TypeVariable(i, kind.toProto)
    case TypeApply(typ, params) =>
      proto.TypeApply(typ.toProto, params.map(_.toProto))
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
    case tyVar @ TypeVariable(_, _) =>
      Set(tyVar)
    case TypeApply(tyVar @ TypeVariable(_, _), params) =>
      params.flatMap(_.freeTypeVariables).toSet + tyVar
    case TypeApply(typ, params) =>
      typ.freeTypeVariables ++ params.flatMap(_.freeTypeVariables).toSet
    case TypeConstructor(_, _) =>
      Set.empty
  }

  def substitute(subst: Map[TypeVariable, Type]): Type = this match {
    case tyVar @ TypeVariable(_, _) =>
      subst.getOrElse(tyVar, tyVar)
    case tyCon @ TypeConstructor(_, _) =>
      tyCon
    case TypeApply(typ, params) =>
      TypeApply(typ.substitute(subst), params.map(_.substitute(subst)))
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
      from :+ to)
  }

  def fromProto(typ: proto.Type): Type = typ match {
    case proto.TypeVariable(id, kind) =>
      TypeVariable(id, Kind.fromProto(kind))
    case proto.TypeConstructor(name, kind) =>
      TypeConstructor(name, Kind.fromProto(kind))
    case proto.TypeApply(typ, params) =>
      TypeApply(Type.fromProto(typ), params.map(Type.fromProto).toList)
    case proto.Type.Empty =>
      throw new Exception("Empty Type in protobuf")
  }
}

case class TypeVariable(id: Int, kind: Kind) extends Type {
  def occursIn(typ: Type) = typ.freeTypeVariables.contains(this)
}

object TypeVariable {
  val nextId = new AtomicInteger(1)
  def apply(kind: Kind = Atomic): TypeVariable =
    TypeVariable(nextId.getAndIncrement, kind)
  def fromProto(tyVar: proto.TypeVariable) =
    TypeVariable(tyVar.id, Atomic)
}

case class TypeConstructor(name: String, kind: Kind) extends Type

case class TypeApply(typ: Type, params: List[Type]) extends Type {
  def kind: Kind = typ.kind match {
    case Parameterized(_, result) =>
      result
    case _ =>
      throw new Exception(s"Type application ${Printer.print(this).render(80)} has unexpected kind ${typ.kind}")
  }
}
