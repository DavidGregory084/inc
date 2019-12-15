package inc.common

import java.lang.Exception
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.{ List, Map }
import scala.{ Int, Product, Serializable }

sealed abstract class Kind extends Product with Serializable {
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
    Parameterized(List.fill(arity)(Atomic), Atomic)

  def fromProto(kind: proto.Kind): Kind = kind match {
    case proto.Atomic() =>
      Atomic
    case proto.Parameterized(params, result) =>
      Parameterized(params.map(Kind.fromProto).toList, Kind.fromProto(result))
    case proto.KindVariable(_) =>
      throw new Exception("Unexpected kind variable in protobuf")
    case proto.Kind.Empty =>
      throw new Exception("Empty Kind in protobuf")
  }
}

case object Atomic extends Kind

case class Parameterized(params: List[Kind], result: Kind) extends Kind

case class KindVariable(id: Int) extends Kind

object KindVariable {
  val nextId = new AtomicInteger(1)
  def apply(): KindVariable = KindVariable(nextId.getAndIncrement)
}

