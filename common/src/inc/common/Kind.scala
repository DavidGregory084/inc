package inc.common

import java.lang.Exception
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.{ List, Map, Set }
import scala.{ Int, Product, Serializable }

sealed abstract class Kind extends Product with Serializable {
  def arity: Int = this match {
    case Atomic =>
      0
    case KindVariable(_) =>
      0
    case Parameterized(params, _) =>
      params.length
  }

  def default: Kind = this match {
    case Atomic =>
      Atomic
    case KindVariable(_) =>
      Atomic
    case Parameterized(params, result) =>
      Parameterized(params.map(_.default), result.default)
  }

  def substitute(subst: Map[KindVariable, Kind]): Kind =
    if (subst.isEmpty)
      this
    else this match {
      case Atomic =>
        Atomic
      case kindVar @ KindVariable(_) =>
        subst.getOrElse(kindVar, kindVar)
      case Parameterized(params, result) =>
        Parameterized(params.map(_.substitute(subst)), result.substitute(subst))
    }

  def kindVariables: Set[KindVariable] = this match {
    case kindVar @ KindVariable(_) =>
      Set(kindVar)
    case Parameterized(params, result) =>
      params.flatMap(_.kindVariables).toSet ++ result.kindVariables
    case Atomic =>
      Set.empty
  }

  def toProto: proto.Kind = this match {
    case Atomic =>
      proto.Atomic()
    case KindVariable(id) =>
      proto.KindVariable(id)
    case Parameterized(params, result) =>
      proto.Parameterized(params.map(_.toProto), result.toProto)
  }
}

object Kind {
  def Function(arity: Int): Kind =
    Parameterized(List.fill(arity)(KindVariable()), Atomic)

  def fromProto(kind: proto.Kind): Kind = kind match {
    case proto.Atomic(_) =>
      Atomic
    case proto.Parameterized(params, result, _) =>
      Parameterized(params.map(Kind.fromProto).toList, Kind.fromProto(result))
    case proto.KindVariable(_, _) =>
      throw new Exception("Unexpected kind variable in protobuf")
    case proto.Kind.Empty =>
      throw new Exception("Empty Kind in protobuf")
  }
}

case object Atomic extends Kind

case class Parameterized(params: List[Kind], result: Kind) extends Kind

case class KindVariable(id: Int) extends Kind {
  def occursIn(kind: Kind) = kind.kindVariables.contains(this)
}

object KindVariable {
  val nextId = new AtomicInteger(1)
  def apply(): KindVariable = KindVariable(nextId.getAndIncrement)
}