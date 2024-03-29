package inc.common

import io.bullet.borer.Codec
import io.bullet.borer.Decoder
import io.bullet.borer.Encoder
import io.bullet.borer.derivation.ArrayBasedCodecs._

import java.lang.String
import scala.collection.immutable.List
import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.immutable.Vector

case class TypeScheme(bound: List[TypeVariable], typ: Type) {
  def toExpr: TypeExpr[Meta.Typed] =
    this.typ.toExpr match {
      case tyCon @ TypeConstructorExpr(_, meta @ Meta.Typed(_, typ, _)) =>
        tyCon.copy(meta = meta.copy(typ = typ.copy(bound = this.bound)))
      case tyApp @ TypeApplyExpr(_, _, meta @ Meta.Typed(_, typ, _)) =>
        tyApp.copy(meta = meta.copy(typ = typ.copy(bound = this.bound)))
    }

  def freeTypeVariables: Set[TypeVariable] =
    typ.freeTypeVariables diff bound.toSet

  def substitute(subst: Map[TypeVariable, Type]) =
    TypeScheme(bound, typ.substitute(subst -- bound))

  def defaultKinds: TypeScheme =
    copy(bound = bound.map(_.defaultKinds.asInstanceOf[TypeVariable]), typ = typ.defaultKinds)

  def substituteKinds(subst: Map[KindVariable, Kind]): TypeScheme =
    copy(
      bound = bound.map(_.substituteKinds(subst).asInstanceOf[TypeVariable]),
      typ = typ.substituteKinds(subst)
    )

  def prefixed(prefix: String) =
    copy(typ = typ.prefixed(prefix))

  def instantiateSubst: Substitution[TypeVariable, Type] =
    if (bound.isEmpty)
      Substitution.empty
    else {
      val freshVars = bound.map(b => TypeVariable(b.kind))
      Substitution(bound.zip(freshVars).toMap)
    }

  def instantiate: Type =
    if (bound.isEmpty)
      typ
    else {
      val freshVars = bound.map(b => TypeVariable(b.kind))
      val subst     = bound.zip(freshVars)
      typ.substitute(subst.toMap)
    }
}

object TypeScheme {
  def apply(typ: Type): TypeScheme = TypeScheme(List.empty, typ)

  def generalize(env: Environment[_], typ: Type): TypeScheme = {
    val freeInEnv = env.types.values.flatMap(_.typ.freeTypeVariables).toSet

    val bound = (typ.freeTypeVariables diff freeInEnv).toList.distinctBy {
      case InferredTypeVariable(id, _) => id.toString
      case NamedTypeVariable(name, _)  => name
    }

    val scheme = TypeScheme(bound.toList, typ)

    scheme
  }

  implicit val typeSchemeSubstitutableTypes: Substitutable[TypeVariable, Type, TypeScheme] =
    new Substitutable[TypeVariable, Type, TypeScheme] {
      def substitute(scheme: TypeScheme, subst: Substitution[TypeVariable, Type]): TypeScheme =
        scheme.substitute(subst.subst)
    }

  implicit val typeSchemeSubstitutableKinds: Substitutable[KindVariable, Kind, TypeScheme] =
    new Substitutable[KindVariable, Kind, TypeScheme] {
      def substitute(scheme: TypeScheme, subst: Substitution[KindVariable, Kind]): TypeScheme =
        scheme.substituteKinds(subst.subst)
    }

  implicit val typeSchemeEncoder: Encoder[TypeScheme] = deriveEncoder[TypeScheme]

  implicit val typeSchemeDecoder: Decoder[TypeScheme] = deriveDecoder[TypeScheme].map { scheme =>
    // Instantiate the type scheme with fresh type variables for this compilation run
    val typ    = scheme.typ
    val tyVars = scheme.bound

    val (freshVars, subst) =
      tyVars.foldLeft((Vector.empty[TypeVariable], Map.empty[TypeVariable, Type])) {
        case ((vars, subst), named @ NamedTypeVariable(_, _)) =>
          (vars :+ named, subst)
        case ((vars, subst), inferred @ InferredTypeVariable(_, kind)) =>
          val freshVar = TypeVariable(kind)
          (vars :+ freshVar, subst.updated(inferred, freshVar))
      }

    TypeScheme(freshVars.toList, typ.substitute(subst))
  }

  implicit val typeSchemeCodec: Codec[TypeScheme] = Codec(typeSchemeEncoder, typeSchemeDecoder)
}
