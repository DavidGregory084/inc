package inc.common

import io.bullet.borer.Codec
import io.bullet.borer.derivation.ArrayBasedCodecs._
import java.lang.String
import scala.{ Product, Serializable }
import scala.collection.immutable.List
import scala.Predef.augmentString

sealed abstract class Import extends Product with Serializable {
  val pos: Pos
  def moduleName: String = this match {
    case ImportModule(pkg, name, _) => pkg.mkString("/") + "/" + name
    case ImportSymbols(pkg, name, _, _) => pkg.mkString("/") + "/" + name
  }
}

object Import {
  implicit val importCodec: Codec[Import] = deriveAllCodecs[Import]
}

case class ImportModule(pkg: List[String], name: String, pos: Pos) extends Import

case class ImportSymbols(pkg: List[String], name: String, symbols: List[String], pos: Pos) extends Import
