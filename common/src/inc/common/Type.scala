package inc.common

import java.lang.{ Exception, String }
import java.util.concurrent.atomic.AtomicInteger
import scala.Int
import scala.collection.immutable.{ List, Map, Set }

sealed trait Type {
  def toProto: proto.Type = this match {
    case TypeVariable(i) => proto.TypeVariable(i)
    case TypeConstructor(name, tyParams) => proto.TypeConstructor(name, tyParams.map(_.toProto))
  }

  def isPrimitive = this match {
    case TypeConstructor(name, _) if Type.primitives.contains(name) =>
      true
    case _ =>
      false
  }

  def freeTypeVariables: Set[TypeVariable] = this match {
    case tyVar @ TypeVariable(_) => Set(tyVar)
    case TypeConstructor(_, tyParams) =>
      tyParams.flatMap(_.freeTypeVariables).toSet
  }

  def substitute(subst: Map[TypeVariable, Type]): Type = this match {
    case tyVar @ TypeVariable(_) =>
      subst.getOrElse(tyVar, tyVar)
    case TypeConstructor(nm, tyParams) =>
      TypeConstructor(nm, tyParams.map(_.substitute(subst)))
  }
}

object Type {
  val UnitClass = "inc.rts.Unit"

  val primitives = Set("Int", "Long", "Float", "Double", "Boolean", "Char")

  val Int = TypeConstructor("Int", List.empty)
  val Long = TypeConstructor("Long", List.empty)
  val Float = TypeConstructor("Float", List.empty)
  val Double = TypeConstructor("Double", List.empty)
  val Boolean = TypeConstructor("Boolean", List.empty)
  val Char = TypeConstructor("Char", List.empty)
  val String = TypeConstructor("String", List.empty)
  val Module = TypeConstructor("Module", List.empty)
  val Unit = TypeConstructor(UnitClass, List.empty)
  def Function(from: List[Type], to: Type) = TypeConstructor("->", from ++ List(to))

  def fromProto(typ: proto.Type): Type = typ match {
    case proto.TypeVariable(id) =>
      TypeVariable(id)
    case proto.TypeConstructor(name, typeParams) =>
      TypeConstructor(name, typeParams.toList.map(Type.fromProto))
    case proto.Type.Empty =>
      throw new Exception("Empty Type in protobuf")
  }
}

case class TypeVariable(id: Int) extends Type {
  def name: String = "T" + id.toString
  def occursIn(typ: Type) = typ.freeTypeVariables.contains(this)
}

object TypeVariable {
  def fromProto(tyVar: proto.TypeVariable) = TypeVariable(tyVar.id)
  val nextVariableId = new AtomicInteger(1)
  def apply(): TypeVariable = TypeVariable(nextVariableId.getAndIncrement)
}

case class TypeConstructor(name: String, typeParams: List[Type]) extends Type
