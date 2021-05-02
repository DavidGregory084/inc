package inc.common

import cats.Functor
import cats.syntax.functor._
import io.bullet.borer.Codec
import io.bullet.borer.derivation.ArrayBasedCodecs._
import java.lang.String
import scala.=:=
import scala.collection.immutable.{ List, Map }
import scala.Predef.augmentString

sealed abstract class TopLevelDeclaration[A] extends SyntaxTree[A] {
  def name: String

  def meta: A

  def members: List[A]

  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed): TopLevelDeclaration[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[TopLevelDeclaration]
      from(this.map(_.substitute(subst)))
    }
  }

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= Meta.Typed): TopLevelDeclaration[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[TopLevelDeclaration]
      from(this.map(_.substituteKinds(subst)))
    }
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): TopLevelDeclaration[A] = {
    val from = to.flip.liftCo[TopLevelDeclaration]
    from(this.map(_.defaultKinds))
  }
}

object TopLevelDeclaration {
  implicit val topLevelDeclarationFunctor: Functor[TopLevelDeclaration] = new Functor[TopLevelDeclaration] {
    def map[A, B](ta: TopLevelDeclaration[A])(f: A => B): TopLevelDeclaration[B] = ta match {
      case let @ Let(_, _, _) =>
        let.copy(
          binding = let.binding.map(f),
          meta = f(let.meta))
      case data @ Data(_, _, _, _) =>
        data.copy(
          typeParams = data.typeParams.map(_.map(f)),
          cases = data.cases.map(_.map(f)),
          meta = f(data.meta))
    }
  }

  implicit val topLevelDeclarationSubstitutableTypes: Substitutable[TypeVariable, Type, TopLevelDeclaration[Meta.Typed]] =
    new Substitutable[TypeVariable, Type, TopLevelDeclaration[Meta.Typed]] {
      def substitute(decl: TopLevelDeclaration[Meta.Typed], subst: Substitution[TypeVariable, Type]): TopLevelDeclaration[Meta.Typed] =
        decl.substitute(subst.subst)
    }

  implicit val topLevelDeclarationSubstitutableKinds: Substitutable[KindVariable, Kind, TopLevelDeclaration[Meta.Typed]] =
    new Substitutable[KindVariable, Kind, TopLevelDeclaration[Meta.Typed]] {
      def substitute(decl: TopLevelDeclaration[Meta.Typed], subst: Substitution[KindVariable, Kind]): TopLevelDeclaration[Meta.Typed] =
        decl.substituteKinds(subst.subst)
    }

  implicit val topLevelDeclarationCodec: Codec[TopLevelDeclaration[Meta.Typed]] = deriveCodec[TopLevelDeclaration[Meta.Typed]]
}

final case class DataConstructor[A](
  name: String,
  params: List[Param[A]],
  meta: A
) extends SyntaxTree[A] {
  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed): DataConstructor[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[DataConstructor]
      from(this.map(_.substitute(subst)))
    }
  }

  def substituteKinds(subst: Map[KindVariable, Kind])(implicit to: A =:= Meta.Typed): DataConstructor[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip.liftCo[DataConstructor]
      from(this.map(_.substituteKinds(subst)))
    }
  }

  def defaultKinds(implicit to: A =:= Meta.Typed): DataConstructor[A] = {
    val from = to.flip.liftCo[DataConstructor]
    from(this.map(_.defaultKinds))
  }
}

object DataConstructor {
  implicit val dataConstructorFunctor: Functor[DataConstructor] = new Functor[DataConstructor] {
    def map[A, B](constr: DataConstructor[A])(f: A => B): DataConstructor[B] = constr match {
      case data @ DataConstructor(_, params, meta) =>
        data.copy(params = params.map(_.map(f)), meta = f(meta))
    }
  }

  implicit val dataConstructorCodec: Codec[DataConstructor[Meta.Typed]] = deriveCodec[DataConstructor[Meta.Typed]]
}

final case class Data[A](
  name: String,
  typeParams: List[TypeConstructorExpr[A]],
  cases: List[DataConstructor[A]],
  meta: A
) extends TopLevelDeclaration[A] {

  def kind(implicit eqv: A =:= Meta.Typed): Kind =
    if (typeParams.isEmpty)
      Atomic
    else
      Parameterized(typeParams.map(_.meta.typ.typ.kind), Atomic)

  def members = meta :: cases.map(_.meta)
}

object Data {
  implicit val dataCodec: Codec[Data[Meta.Typed]] = deriveCodec[Data[Meta.Typed]]
}

final case class Let[A](
  name: String,
  binding: Expr[A],
  meta: A
) extends TopLevelDeclaration[A] {
  def members = List(meta)
}

object Let {
  implicit val letCodec: Codec[Let[Meta.Typed]] = deriveCodec[Let[Meta.Typed]]
}
