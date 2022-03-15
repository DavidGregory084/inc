package inc.common

import io.bullet.borer.Codec
import io.bullet.borer.derivation.ArrayBasedCodecs._

import java.util.concurrent.atomic.AtomicInteger
import scala.Int
import scala.Predef.augmentString
import scala.Product
import scala.Serializable
import scala.collection.immutable.List
import scala.collection.immutable.Map
import scala.collection.immutable.Set

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
    else
      this match {
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
}

object Kind {
  def Function(arity: Int): Kind =
    Parameterized(List.fill(arity)(KindVariable()), Atomic)

  implicit val kindSubstitutableKinds: Substitutable[KindVariable, Kind, Kind] =
    new Substitutable[KindVariable, Kind, Kind] {
      def substitute(kind: Kind, subst: Substitution[KindVariable, Kind]): Kind =
        kind.substitute(subst.subst)
    }
  implicit val kindCodec: Codec[Kind] = deriveAllCodecs[Kind]
}

case object Atomic extends Kind

case class Parameterized(params: List[Kind], result: Kind) extends Kind

case class KindVariable(id: Int) extends Kind {
  def occursIn(kind: Kind) = kind.kindVariables.contains(this)
}

object KindVariable {
  val nextId                = new AtomicInteger(1)
  def apply(): KindVariable = KindVariable(nextId.getAndIncrement)
}
