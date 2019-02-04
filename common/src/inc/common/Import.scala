package inc.common

import java.lang.String
import scala.{ Some, None }
import scala.collection.immutable.List

sealed trait Import {
  def toProto: proto.Import = this match {
    case ImportModule(pkg, name) => proto.Import(pkg, name)
    case ImportSymbols(pkg, name, symbols) => proto.Import(pkg, name, Some(proto.Symbols(symbols)))
  }
}

object Import {
  def fromProto(imp: proto.Import): Import = imp.symbols match {
    case Some(syms) =>
      ImportSymbols(imp.pkg.toList, imp.name, syms.symbols.toList)
    case None =>
      ImportModule(imp.pkg.toList, imp.name)
  }
}

case class ImportModule(pkg: List[String], name: String) extends Import

case class ImportSymbols(pkg: List[String], name: String, symbols: List[String]) extends Import