package inc.common

import cats.Functor
import cats.syntax.functor._
import java.lang.{ Exception, String }
import scala.{ Boolean, Option, Some, None, =:= }
import scala.collection.immutable.{ List, Set }

sealed abstract class Pattern[A] extends SyntaxTree[A] {
  def meta: A

  def isIrrefutable: Boolean = this match {
    case IdentPattern(_, _) =>
      true
    case ConstrPattern(_, _, _, _) =>
      false
  }

  def boundVariables(implicit eqv: A =:= Meta.Typed): Set[Name] = this match {
    case IdentPattern(_, meta) =>
      Set(meta.name)
    case ConstrPattern(_, _, patterns, meta) =>
      (Set(meta.name) ++ patterns.flatMap(_.boundVariables)).filterNot(_ == NoName)
  }

  def toProto(implicit eqv: A =:= Meta.Typed): proto.Pattern = this match {
    case IdentPattern(name, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.IdentPattern(name, typedMeta)
    case ConstrPattern(name, alias, patterns, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.ConstrPattern(name, alias.getOrElse(""), patterns.map(_.toProto), typedMeta)
  }
}


case class IdentPattern[A](name: String, meta: A) extends Pattern[A]

case class FieldPattern[A](name: String, pattern: Option[Pattern[A]], meta: A) extends SyntaxTree[A] {
  def boundVariables(implicit eqv: A =:= Meta.Typed): Set[Name] =
    pattern.toList.flatMap(_.boundVariables).toSet
  def toProto(implicit eqv: A =:= Meta.Typed): proto.FieldPattern = {
    val typedMeta = Some(eqv(meta).toProto)
    proto.FieldPattern(name, pattern.map(_.toProto).getOrElse(proto.Pattern.Empty), typedMeta)
  }
}

object FieldPattern {
  implicit val fieldPatternFunctor: Functor[FieldPattern] = new Functor[FieldPattern] {
    def map[A, B](pa: FieldPattern[A])(f: A => B): FieldPattern[B] = pa match {
      case FieldPattern(field, pattern, meta) =>
        FieldPattern(field, pattern.map(_.map(f)), f(meta))
    }
  }
  def fromProto(pat: proto.FieldPattern): FieldPattern[Meta.Typed] = pat match {
    case fld @ proto.FieldPattern(field, pattern, _, _) =>
      pattern match {
        case proto.Pattern.Empty =>
          FieldPattern(field, None, Meta.fromProto(fld.getNameWithType))
        case other =>
          FieldPattern(field, Some(Pattern.fromProto(other)), Meta.fromProto(fld.getNameWithType))
      }
  }
}

case class ConstrPattern[A](name: String, alias: Option[String], patterns: List[FieldPattern[A]], meta: A) extends Pattern[A]

object Pattern {
  def fromProto(pat: proto.Pattern): Pattern[Meta.Typed] = pat match {
    case id @ proto.IdentPattern(name, _, _) =>
      IdentPattern(name, Meta.fromProto(id.getNameWithType))
    case constr @ proto.ConstrPattern(name, alias, patterns, _, _) =>
      ConstrPattern(
        name,
        Option(alias).filterNot(_.isEmpty),
        patterns.map(FieldPattern.fromProto).toList,
        Meta.fromProto(constr.getNameWithType))
    case proto.Pattern.Empty =>
      throw new Exception("Empty Pattern in protobuf")
  }

  implicit val patternFunctor: Functor[Pattern] = new Functor[Pattern] {
    def map[A, B](pa: Pattern[A])(f: A => B): Pattern[B] = pa match {
      case ident @ IdentPattern(_, _) =>
        ident.copy(meta = f(ident.meta))
      case constr @ ConstrPattern(_, _, _, _) =>
        constr.copy(
          patterns = constr.patterns.map(_.map(f)),
          meta = f(constr.meta))
    }
  }
}
