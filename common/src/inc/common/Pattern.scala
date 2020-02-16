package inc.common

import cats.Functor
import java.lang.{ Exception, String }
import scala.{ Product, Serializable, Some, =:= }
import scala.collection.immutable.List

sealed abstract class Pattern[A] extends Product with Serializable {
  def meta: A

  def toProto(implicit eqv: A =:= Meta.Typed): proto.Pattern = this match {
    case IdentPattern(name, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.IdentPattern(name, typedMeta)
    case AliasPattern(pattern, alias, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.AliasPattern(pattern.toProto, alias, typedMeta)
    case ConstrPattern(name, patterns, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.ConstrPattern(name, patterns.map(_.toProto), typedMeta)
  }
}

case class IdentPattern[A](name: String, meta: A) extends Pattern[A]
case class AliasPattern[A](pattern: Pattern[A], alias: String, meta: A) extends Pattern[A]
case class ConstrPattern[A](name: String, patterns: List[Pattern[A]], meta: A) extends Pattern[A]

object Pattern {
  def fromProto(pat: proto.Pattern): Pattern[Meta.Typed] = pat match {
    case id @ proto.IdentPattern(name, _) =>
      IdentPattern(name, Meta.fromProto(id.getNameWithType))
    case al @ proto.AliasPattern(pattern, alias, _) =>
      AliasPattern(Pattern.fromProto(pattern), alias, Meta.fromProto(al.getNameWithType))
    case constr @ proto.ConstrPattern(name, patterns, _) =>
      ConstrPattern(
        name,
        patterns.map(Pattern.fromProto).toList,
        Meta.fromProto(constr.getNameWithType))
    case proto.Pattern.Empty =>
      throw new Exception("Empty Pattern in protobuf")
  }

  implicit val patternFunctor: Functor[Pattern] = new Functor[Pattern] {
    def map[A, B](pa: Pattern[A])(f: A => B): Pattern[B] = pa match {
      case ident @ IdentPattern(_, _) =>
        ident.copy(meta = f(ident.meta))
      case alias @ AliasPattern(_, _, _) =>
        alias.copy(
          pattern = map(alias.pattern)(f),
          meta = f(alias.meta))
      case constr @ ConstrPattern(_, _, _) =>
        constr.copy(
          patterns = constr.patterns.map(map(_)(f)),
          meta = f(constr.meta))
    }
  }
}
