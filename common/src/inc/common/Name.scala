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
}

object Name {
  def fromProto(name: proto.Name): Name = name match {
    case proto.NoName() => NoName
    case proto.LocalName(nm) => LocalName(nm)
    case proto.ModuleName(pkg, mod) => ModuleName(pkg.toList, mod)
    case proto.MemberName(pkg, mod, nm) => MemberName(pkg.toList, mod, nm)
    case proto.DataName(pkg, mod, nm) => DataName(pkg.toList, mod, nm)
    case proto.ConstrName(pkg, mod, data, nm) => ConstrName(pkg.toList, mod, data, nm)
    case proto.Name.Empty => throw new Exception("Empty Name in protobuf")
  }
}

case object NoName extends Name
case class LocalName(name: String) extends Name
case class ModuleName(pkg: List[String], mod: String) extends Name
case class MemberName(pkg: List[String], mod: String, name: String) extends Name {
  def fullName =
    if (pkg.isEmpty)
      mod + "." + name
    else
      pkg.mkString("/") + "/" + mod + "." + name
}
case class DataName(pkg: List[String], mod: String, name: String) extends Name
case class ConstrName(pkg: List[String], mod: String, data: String, name: String) extends Name
