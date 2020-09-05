package inc.common

import cats.Functor
import cats.syntax.functor._
import java.lang.String
import scala.Some
import scala.=:=
import scala.collection.immutable.{ List, Map }
import scala.Predef.ArrowAssoc

final case class Module[A](
  pkg: List[String],
  name: String,
  imports: List[Import],
  declarations: List[TopLevelDeclaration[A]],
  meta: A,
) {

  override def toString = pprint.apply(this).toString()

  def toProto(implicit eqv: A =:= Meta.Typed): proto.Module = proto.Module(
    pkg = pkg,
    name = name,
    imports = imports.map(_.toProto),
    declarations = declarations.map(_.toProto),
    nameWithType = Some(eqv(meta).toProto)
  )

  def fullName = (pkg :+ name).mkString("/")

  def symbolTable(implicit eqv: A =:= Meta.Untyped): Environment[A] = {
    val to = eqv.liftCo[TopLevelDeclaration]
    val envFrom = eqv.liftCo[Environment].flip
    val namedDeclarations = declarations.map(to.apply)

    val names = namedDeclarations.flatMap {
      case Let(name, _, meta) =>
        List(name -> meta.name)
      case Data(_, _, cases, _) =>
        cases.map {
          case DataConstructor(caseName, _, caseMeta) =>
            caseName -> caseMeta.name
        }
    }

    val typeNames = namedDeclarations.flatMap {
      case Data(name, _, _, meta) =>
        List(name -> meta.name)
      case _ =>
        List.empty
    }

    val members = namedDeclarations.flatMap {
      case Data(_, _, cases, dataMeta) =>
        val dataMembers = for {
          DataConstructor(_, params, constrMeta) <- cases
          dataMember = dataMeta.name -> constrMeta
        } yield dataMember

        val constrMembers = for {
          DataConstructor(_, params, constrMeta) <- cases
          Param(_, _, paramMeta) <- params
          constrMember = constrMeta.name -> paramMeta
        } yield constrMember

        val emptyConstrs = for {
          DataConstructor(name, params, constrMeta) <- cases
          if params.isEmpty
        } yield constrMeta.name -> List.empty[Meta.Untyped]

        val allMembers = dataMembers ++ constrMembers

        allMembers.groupMap(_._1)(_._2) ++ emptyConstrs

      case _ =>
        List.empty
    }

    val env = Environment.empty
      .withValueNames(names)
      .withTypeNames(typeNames)
      .copy(members = members.toMap)

    envFrom(env)
  }

  def typeEnvironment(implicit eqv: A =:= Meta.Typed): Environment[A] = {
    val to = eqv.liftCo[TopLevelDeclaration]
    val envFrom = eqv.liftCo[Environment].flip

    val typedDeclarations = declarations.map(to.apply)

    val names = typedDeclarations.flatMap {
      case Let(name, _, meta) =>
        List(name -> meta.name)
      case Data(_, _, cases, _) =>
        cases.map {
          case DataConstructor(caseName, _, caseMeta) =>
            caseName -> caseMeta.name
        }
    }

    val typeNames = typedDeclarations.flatMap {
      case Data(name, _, _, meta) =>
        List(name -> meta.name)
      case _ =>
        List.empty
    }

    val types = typedDeclarations.flatMap {
      case Let(name, _, meta) =>
        List(name -> meta.typ)
      case Data(_, _, cases, _) =>
        cases.map {
          case DataConstructor(name, _, meta) =>
            name -> meta.typ
        }
    }

    val members = typedDeclarations.flatMap {
      case Data(_, _, cases, dataMeta) =>
        val dataMembers = for {
          DataConstructor(_, params, constrMeta) <- cases
          dataMember = dataMeta.name -> constrMeta
        } yield dataMember

        val constrMembers = for {
          DataConstructor(_, params, constrMeta) <- cases

          Param(_, ascribedAs, paramMeta) <- params

          fnType = TypeScheme(
            dataMeta.typ.bound,
            Type.Function(List(dataMeta.typ.typ), ascribedAs.get.meta.typ.typ)
          )

          constrMember = constrMeta.name -> paramMeta.copy(typ = fnType)

        } yield constrMember

        val emptyConstrs = for {
          DataConstructor(name, params, constrMeta) <- cases
          if params.isEmpty
        } yield constrMeta.name -> List.empty[Meta.Typed]

        val allMembers = dataMembers ++ constrMembers

        allMembers.groupMap(_._1)(_._2) ++ emptyConstrs

      case _ =>
        List.empty
    }

    val kinds = typedDeclarations.collect {
      case Data(name, _, _, meta) =>
        name -> meta.typ.typ.kind
    }

    val env = Environment.empty
      .withValueNames(names)
      .withTypeNames(typeNames)
      .withTypes(types)
      .withKinds(kinds)
      .copy(members = members.toMap)

    envFrom(env)
  }

  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed): Module[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip
      val typedMeta = from(meta.substitute(subst))
      copy(
        declarations = declarations.map(_.substitute(subst)),
        meta = typedMeta)
    }
  }

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= Meta.Typed): Module[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip
      val typedMeta = from(meta.substituteKinds(subst))
      copy(
        declarations = declarations.map(_.substituteKinds(subst)),
        meta = typedMeta)
    }
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): Module[A] = {
    val from = to.flip
    val typedMeta = from(meta.defaultKinds)
    copy(
      declarations = declarations.map(_.defaultKinds),
      meta = typedMeta)
  }
}

object Module {
  def fromProto(mod: proto.Module): Module[Meta.Typed] = Module(
    pkg = mod.pkg.toList,
    name = mod.name,
    imports = mod.imports.toList.map(Import.fromProto),
    declarations = mod.declarations.toList.map(TopLevelDeclaration.fromProto),
    meta = Meta.fromProto(mod.getNameWithType),
  )
  implicit val moduleFunctor: Functor[Module] = new Functor[Module] {
    def map[A, B](ma: Module[A])(f: A => B): Module[B] = {
      ma.copy(
        meta = f(ma.meta),
        declarations = ma.declarations.map(_.map(f))
      )
    }
  }

  implicit val moduleSubstitutableTypes: Substitutable[TypeVariable, Type, Module[Meta.Typed]] = new Substitutable[TypeVariable, Type, Module[Meta.Typed]] {
    def substitute(mod: Module[Meta.Typed], subst: Substitution[TypeVariable, Type]): Module[Meta.Typed] =
      mod.substitute(subst.subst)
  }

  implicit val moduleSubstitutableKinds: Substitutable[KindVariable, Kind, Module[Meta.Typed]] = new Substitutable[KindVariable, Kind, Module[Meta.Typed]] {
    def substitute(mod: Module[Meta.Typed], subst: Substitution[KindVariable, Kind]): Module[Meta.Typed] =
      mod.substituteKinds(subst.subst)
  }
}
