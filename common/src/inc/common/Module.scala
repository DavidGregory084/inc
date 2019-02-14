package inc.common

import cats.Functor
import cats.syntax.functor._
import java.lang.String
import scala.Some
import scala.Predef.=:=
import scala.collection.immutable.{ List, Map }

final case class Module[A](
  pkg: List[String],
  name: String,
  imports: List[Import],
  declarations: List[TopLevelDeclaration[A]],
  meta: A,
) {
  def toProto(implicit eqv: A =:= NamePosType): proto.Module = proto.Module(
    pkg = pkg,
    name = name,
    imports = imports.map(_.toProto),
    declarations = declarations.map(_.toProto),
    nameWithType = Some(eqv(meta).toProto)
  )

  // def substitute(subst: Map[TypeVariable, Type])(implicit eqv: A =:= NamePosType): Module[A] =
  //   this.map(a => eqv(a).substitute(subst).asInstanceOf[A])
}

object Module {
  def fromProto(mod: proto.Module): Module[NameWithType] = Module(
    pkg = mod.pkg.toList,
    name = mod.name,
    imports = mod.imports.toList.map(Import.fromProto),
    declarations = mod.declarations.toList.map(TopLevelDeclaration.fromProto),
    meta = NameWithType.fromProto(mod.getNameWithType),
  )
  // implicit val moduleFunctor: Functor[Module] = new Functor[Module] {
  //   def map[A, B](ma: Module[A])(f: A => B): Module[B] = {
  //     ma.copy(
  //       meta = f(ma.meta),
  //       declarations = ma.declarations.map(_.map(f))
  //     )
  //   }
  // }
}
