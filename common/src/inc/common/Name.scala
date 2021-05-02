package inc.common

import io.bullet.borer._
import io.bullet.borer.derivation.ArrayBasedCodecs._
import java.lang.String
import scala.collection.immutable.List
import scala.{ Product, Serializable }
import scala.Predef.augmentString

sealed abstract class Name extends Product with Serializable {
  def shortName: String = this match {
    case NoName => ""
    case LocalName(nm) => nm
    case DataName(_, _, nm) => nm
    case ModuleName(_, mod) => mod
    case MemberName(_, _, nm) => nm
    case ConstrName(_, _, _, nm) => nm
  }

  def fullName: String = this match {
    case NoName =>
      ""
    case LocalName(nm) =>
      nm
    case ModuleName(pkg, mod) =>
      if (pkg.isEmpty) mod else pkg.mkString("/") + "/" + mod
    case MemberName(pkg, mod, nm) =>
      if (pkg.isEmpty) mod + "." + nm else pkg.mkString("/") + "/" + mod + "." + nm
    case DataName(pkg, mod, nm) =>
      if (pkg.isEmpty) mod + "." + nm else pkg.mkString("/") + "/" + mod + "." + nm
    case ConstrName(pkg, mod, _, nm) =>
      if (pkg.isEmpty) mod + "." + nm else pkg.mkString("/") + "/" + mod + "." + nm
  }
}

case object NoName extends Name
case class LocalName(name: String) extends Name
case class ModuleName(pkg: List[String], mod: String) extends Name
case class MemberName(pkg: List[String], mod: String, name: String) extends Name
case class DataName(pkg: List[String], mod: String, name: String) extends Name
case class ConstrName(pkg: List[String], mod: String, data: String, name: String) extends Name

object Name {
  implicit val nameCodec: Codec[Name] = deriveAllCodecs[Name]
}
