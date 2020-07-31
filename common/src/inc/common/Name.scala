package inc.common

import java.lang.{ Exception, String }
import scala.collection.immutable.List
import scala.{ Product, Serializable }

sealed abstract class Name extends Product with Serializable {
  def toProto: proto.Name = this match {
    case NoName => proto.NoName()
    case LocalName(nm) => proto.LocalName(nm)
    case ModuleName(pkg, mod) => proto.ModuleName(pkg, mod)
    case MemberName(pkg, mod, nm) => proto.MemberName(pkg, mod, nm)
    case DataName(pkg, mod, nm) => proto.DataName(pkg, mod, nm)
    case ConstrName(pkg, mod, data, nm) => proto.ConstrName(pkg, mod, data, nm)
  }

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

object Name {
  def fromProto(name: proto.Name): Name = name match {
    case proto.NoName(_) => NoName
    case proto.LocalName(nm, _) => LocalName(nm)
    case proto.ModuleName(pkg, mod, _) => ModuleName(pkg.toList, mod)
    case proto.MemberName(pkg, mod, nm, _) => MemberName(pkg.toList, mod, nm)
    case proto.DataName(pkg, mod, nm, _) => DataName(pkg.toList, mod, nm)
    case proto.ConstrName(pkg, mod, data, nm, _) => ConstrName(pkg.toList, mod, data, nm)
    case proto.Name.Empty => throw new Exception("Empty Name in protobuf")
  }
}

case object NoName extends Name
case class LocalName(name: String) extends Name
case class ModuleName(pkg: List[String], mod: String) extends Name
case class MemberName(pkg: List[String], mod: String, name: String) extends Name
case class DataName(pkg: List[String], mod: String, name: String) extends Name
case class ConstrName(pkg: List[String], mod: String, data: String, name: String) extends Name
