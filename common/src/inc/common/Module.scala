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
  def toProto(implicit eqv: A =:= Meta.Typed): proto.Module = proto.Module(
    pkg = pkg,
    name = name,
    imports = imports.map(_.toProto),
    declarations = declarations.map(_.toProto),
    nameWithType = Some(eqv(meta).toProto)
  )

  def fullName = (pkg :+ name).mkString("/")

  def environment(implicit eqv: A =:= Meta.Typed): Environment = {
    val to = eqv.liftCo[TopLevelDeclaration]

    val typedDeclarations = declarations.map(to.apply)

    val names = typedDeclarations.flatMap {
      case Let(name, _, meta) =>
        List(name -> meta.name)
      case Data(name, _, cases, meta) =>
        (name -> meta.name) :: cases.map {
          case DataConstructor(caseName, _, _, caseMeta) =>
            caseName -> caseMeta.name
        }
    }

    val types = typedDeclarations.flatMap {
      case Let(name, _, meta) =>
        List(name -> meta.typ)
      case Data(_, _, cases, _) =>
        cases.map {
          case DataConstructor(name, _, _, meta) =>
            name -> meta.typ
        }
    }

    val kinds = typedDeclarations.collect {
      case Data(name, _, _, meta) =>
        name -> meta.typ.typ.kind
    }

    Environment(names.toMap, types.toMap, kinds.toMap)
  }

  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed): Module[A] =
    if (subst.isEmpty)
      this
    else {
      val from = to.flip
      this.map(a => from(to(a).substitute(subst)))
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
}
