package inc.common

import java.lang.String
import scala.collection.immutable.{ List, Map, Set }

case class TypeScheme(bound: List[TypeVariable], typ: Type) {
  def toProto: proto.TypeScheme =
    proto.TypeScheme(bound.map(tv => proto.TypeVariable(tv.id)), typ.toProto)

  def freeTypeVariables: Set[TypeVariable] =
    typ.freeTypeVariables diff bound.toSet

  def substitute(subst: Map[TypeVariable, Type]) =
    TypeScheme(bound, typ.substitute(subst -- bound))

  def instantiate: (Type, Map[TypeVariable, Type]) =
    if (bound.isEmpty)
      (typ, Map.empty)
    else {
      val freshVars = bound.map(_ => TypeVariable())
      val subst = bound.zip(freshVars).toMap
      scribe.info(NL + "Instantiate: " + Printer.print(subst))
      (typ.substitute(subst), subst)
    }
}

object TypeScheme {
  def apply(typ: Type): TypeScheme = TypeScheme(List.empty, typ)

  def generalize(env: Map[String, TypeScheme], typ: Type): TypeScheme = {
    val freeInEnv = env.values.flatMap(_.freeTypeVariables).toSet
    val bound = typ.freeTypeVariables diff freeInEnv
    val scheme = TypeScheme(bound.toList, typ)
    if (bound.nonEmpty) scribe.info(NL + "Generalize: " + bound.map(Printer.print(_)).mkString("[", ", ", "]"))
    scheme
  }

  def fromProto(typ: proto.TypeScheme) = typ match {
    case proto.TypeScheme(bound, t) =>
      // Instantiate the type scheme with fresh type variables for this compilation run
      val typ = Type.fromProto(t)
      val freshVars = bound.toList.map(_ => TypeVariable())
      val tyVars = bound.toList.map(TypeVariable.fromProto)
      val subst = tyVars.zip(freshVars).toMap
      TypeScheme(tyVars, typ.substitute(subst))
  }
}
