package inc.typechecker

import inc.common._
import java.lang.String
import scala.{ Either, Left, Nothing, StringContext }
import scala.collection.immutable.List

sealed abstract class TypeError(private val position: Pos) extends Error(position)

case class TypeUnificationError(position: Pos, left: Type, right: Type)(implicit file: sourcecode.File, line: sourcecode.Line) extends TypeError(position) {
  val message: String = {
    val leftStr = Printer.print(left).render(80)
    val rightStr = Printer.print(right).render(80)
    val msg = s"${Red(leftStr)} does not unify with ${Red(rightStr)}"
    Error.formatMessage(file, line, msg)
  }
}

case class TypeOccursCheck(position: Pos, tyVar: TypeVariable, typ: Type)(implicit file: sourcecode.File, line: sourcecode.Line) extends TypeError(position) {
  val message: String = {
    val typStr = Printer.print(typ).render(80)
    val tyVarStr = Printer.print(tyVar).render(80)
    val msg = s"Infinite expansion while unifying type variable ${Red(tyVarStr)} with type ${Red(typStr)}"
    Error.formatMessage(file, line, msg)
  }
}

case class TypeApplicationError(position: Pos, left: Type, right: Type)(implicit file: sourcecode.File, line: sourcecode.Line) extends TypeError(position) {
  val message: String = {
    val leftStr = Printer.print(left).render(80)
    val rightStr = Printer.print(right).render(80)

    val msg = s"${Red(leftStr)} does not unify with ${Red(rightStr)}: ${leftStr} has arity ${left.kind.arity}; ${rightStr} has arity ${right.kind.arity}"

    Error.formatMessage(file, line, msg)
  }
}

case class KindUnificationError(position: Pos, left: Kind, right: Kind)(implicit file: sourcecode.File, line: sourcecode.Line) extends TypeError(position) {
  val message: String = {
    val leftStr = Printer.print(left).render(80)
    val rightStr = Printer.print(right).render(80)
    val msg = s"${Red(leftStr)} does not unify with ${Red(rightStr)}"
    Error.formatMessage(file, line, msg)
  }
}

case class KindOccursCheck(position: Pos, kindVar: KindVariable, kind: Kind)(implicit file: sourcecode.File, line: sourcecode.Line) extends TypeError(position) {
  val message: String = {
    val kindStr = Printer.print(kind).render(80)
    val kindVarStr = Printer.print(kindVar).render(80)
    val msg = s"Infinite expansion while unifying kind variable ${Red(kindVarStr)} with kind ${Red(kindStr)}"
    Error.formatMessage(file, line, msg)
  }

}

case class GenericError(position: Pos, message: String) extends TypeError(position)

object TypeError {
  def typeUnification(pos: Pos, left: Type, right: Type)(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[TypeError], Nothing] =
    Left(List(TypeUnificationError(pos, left, right)))
  def typeOccursCheck(pos: Pos, tyVar: TypeVariable, typ: Type)(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[TypeError], Nothing] =
    Left(List(TypeOccursCheck(pos, tyVar, typ)))
  def typeApplication(pos: Pos, left: Type, right: Type)(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[TypeError], Nothing] =
    Left(List(TypeApplicationError(pos, left, right)))
  def kindUnification(pos: Pos, left: Kind, right: Kind)(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[TypeError], Nothing] =
    Left(List(KindUnificationError(pos, left, right)))
  def kindOccursCheck(pos: Pos, kindVar: KindVariable, kind: Kind)(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[TypeError], Nothing] =
    Left(List(KindOccursCheck(pos, kindVar, kind)))
  def generic(pos: Pos, msg: String)(implicit file: sourcecode.File, line: sourcecode.Line): Either[List[TypeError], Nothing] = {
    Left(List(GenericError(pos,  Error.formatMessage(file, line, msg))))
  }
}
