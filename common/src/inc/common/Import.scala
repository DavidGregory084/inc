package inc.common

import java.lang.String
import scala.{ Some, None }
import scala.collection.immutable.List

sealed trait Import {
  val pos: Pos
  def moduleName: String = this match {
    case ImportModule(pkg, name, _) => pkg.mkString("/") + "/" + name
    case ImportSymbols(pkg, name, _, _) => pkg.mkString("/") + "/" + name
  }
  def toProto: proto.Import = this match {
    case ImportModule(pkg, name, _) => proto.Import(pkg, name)
    case ImportSymbols(pkg, name, symbols, _) => proto.Import(pkg, name, Some(proto.Symbols(symbols)))
  }
}

object Import {
  def fromProto(imp: proto.Import): Import = imp.symbols match {
    case Some(syms) =>
      ImportSymbols(imp.pkg.toList, imp.name, syms.symbols.toList, Pos.Empty)
    case None =>
      ImportModule(imp.pkg.toList, imp.name, Pos.Empty)
  }
}

case class ImportModule(pkg: List[String], name: String, pos: Pos) extends Import

case class ImportSymbols(pkg: List[String], name: String, symbols: List[String], pos: Pos) extends Import
