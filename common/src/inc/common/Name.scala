package inc.common

import java.lang.{ Exception, String }
import scala.collection.immutable.List
import scala.{ Product, Serializable }

sealed abstract class Name extends Product with Serializable {
  def toProto: proto.Name = this match {
    case NoName => proto.NoName()
    case LocalName(nm) => proto.LocalName(nm)
    case ModuleName(pkg, cls) => proto.ModuleName(pkg, cls)
    case MemberName(pkg, cls, nm) => proto.MemberName(pkg, cls, nm)
  }
}

object Name {
  def fromProto(name: proto.Name): Name = name match {
    case proto.NoName() => NoName
    case proto.LocalName(nm) => LocalName(nm)
    case proto.ModuleName(pkg, cls) => ModuleName(pkg.toList, cls)
    case proto.MemberName(pkg, cls, nm) => MemberName(pkg.toList, cls, nm)
    case proto.Name.Empty => throw new Exception("Empty Name in protobuf")
  }
}

case object NoName extends Name
case class LocalName(name: String) extends Name
case class ModuleName(pkg: List[String], cls: String) extends Name
case class MemberName(pkg: List[String], cls: String, name: String) extends Name
