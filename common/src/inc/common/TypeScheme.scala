package inc.common

import java.lang.String
import scala.collection.immutable.{ List, Map, Set, Vector }

case class TypeScheme(bound: List[TypeVariable], typ: Type) {
  def toProto: proto.TypeScheme =
    proto.TypeScheme(bound.map(tv => tv.toProto), typ.toProto)

  def freeTypeVariables: Set[TypeVariable] =
    typ.freeTypeVariables diff bound.toSet

  def replace(subst: Map[String, TypeVariable]) =
    TypeScheme(bound, typ.replace(subst))

  def substitute(subst: Map[TypeVariable, Type]) =
    TypeScheme(bound, typ.substitute(subst -- bound))

  def instantiate: Type =
    if (bound.isEmpty)
      typ
    else {
      val freshVars = bound.map(_ => TypeVariable())
      val subst = bound.zip(freshVars)
      typ.substitute(subst.toMap)
    }
}

object TypeScheme {
  def apply(typ: Type): TypeScheme = TypeScheme(List.empty, typ)

  def generalize(env: Map[String, TypeScheme], typ: Type): TypeScheme = {
    val freeInEnv = env.values.flatMap(_.freeTypeVariables).toSet
    val bound = typ.freeTypeVariables diff freeInEnv
    val scheme = TypeScheme(bound.toList, typ)
    scheme
  }

  def fromProto(typ: proto.TypeScheme) = typ match {
    case proto.TypeScheme(bound, t) =>
      // Instantiate the type scheme with fresh type variables for this compilation run
      val typ = Type.fromProto(t)

      val tyVars = bound.toList.map(TypeVariable.fromProto)

      val (freshVars, subst) = tyVars.foldLeft((Vector.empty[TypeVariable], Map.empty[TypeVariable, Type])) {
        case ((vars, subst), named @ NamedTypeVariable(_, _)) =>
          (vars :+ named, subst)
        case ((vars, subst), inferred @ InferredTypeVariable(_, kind)) =>
          val freshVar = TypeVariable(kind)
          (vars :+ freshVar, subst.updated(inferred, freshVar))
      }

      TypeScheme(freshVars.toList, typ.substitute(subst))
  }
}
