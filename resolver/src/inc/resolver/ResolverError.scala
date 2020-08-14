package inc.resolver

import inc.common.{ Error, Pos, Name }
import java.lang.String
import scala.{ Either, Left, Nothing, StringContext }
import scala.collection.immutable.List

sealed abstract class ResolverError(private val position: Pos) extends Error(position)

case class UnknownMemberError(position: Pos, constr: String, member: String)(implicit file: sourcecode.File, line: sourcecode.Line) extends ResolverError(position) {
  val message: String = {
    val msg = s"Constructor ${constr} has no member ${member}"
    Error.formatMessage(file, line, msg)
  }
}

case class UnknownConstructorError(position: Pos, name: String)(implicit file: sourcecode.File, line: sourcecode.Line) extends ResolverError(position) {
  val message: String = {
    val msg = s"Reference to unknown constructor: ${name}"
    Error.formatMessage(file, line, msg)
  }
}

case class UndefinedSymbolError(position: Pos, name: String)(implicit file: sourcecode.File, line: sourcecode.Line) extends ResolverError(position) {
  val message: String = {
    val msg = s"Reference to undefined symbol: ${name}"
    Error.formatMessage(file, line, msg)
  }
}

case class AlreadyDefinedSymbolError(position: Pos, name: String, existing: Name)(implicit file: sourcecode.File, line: sourcecode.Line) extends ResolverError(position) {
  val message: String = {
    val msg = s"Symbol ${name} is already defined as ${existing}"
    Error.formatMessage(file, line, msg)
  }
}

case class GenericError(position: Pos, message: String) extends ResolverError(position)

object ResolverError {
  def unknownMember(pos: Pos, constr: String, member: String)(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[ResolverError], Nothing] = {
    Left(List(UnknownMemberError(pos, constr, member)))
  }
  def unknownConstructor(pos: Pos, name: String)(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[ResolverError], Nothing] = {
    Left(List(UnknownConstructorError(pos, name)))
  }
  def undefined(pos: Pos, name: String)(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[ResolverError], Nothing] = {
    Left(List(UndefinedSymbolError(pos, name)))
  }
  def alreadyDefined(pos: Pos, name: String, existing: Name)(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[ResolverError], Nothing] = {
    Left(List(AlreadyDefinedSymbolError(pos, name, existing)))
  }
  def generic(pos: Pos, msg: String)(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[ResolverError], Nothing] = {
    Left(List(GenericError(pos, Error.formatMessage(file, line, msg))))
  }
}
