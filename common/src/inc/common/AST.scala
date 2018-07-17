package inc.common

abstract class Error(val msg: String) extends Throwable(msg) with Product with Serializable

sealed trait Tree[A] extends Product with Serializable {
  def meta: A
}

final case class Module[A](
  pkg: Seq[String],
  name: String,
  imports: Seq[Import],
  declarations: Seq[TopLevelDeclaration[A]],
  meta: A
) extends Tree[A]

sealed trait Import

case class ImportModule(pkg: Seq[String], name: String) extends Import
case class ImportSymbols(pkg: Seq[String], name: String, symbols: Seq[String]) extends Import

sealed trait Expr[A] extends Tree[A]

// case class Block(decls: Seq[LocalDeclaration], result: Expr) extends Expr

sealed trait Literal[A] extends Expr[A]

final case class LiteralInt[A](i: Int, meta: A) extends Literal[A]
final case class LiteralLong[A](l: Long, meta: A) extends Literal[A]
final case class LiteralFloat[A](f: Float, meta: A) extends Literal[A]
final case class LiteralDouble[A](d: Double, meta: A) extends Literal[A]
final case class LiteralBoolean[A](b: Boolean, meta: A) extends Literal[A]
final case class LiteralChar[A](c: Char, meta: A) extends Literal[A]
final case class LiteralString[A](s: String, meta: A) extends Literal[A]

final case class Reference[A](name: String, meta: A) extends Expr[A]

sealed trait Declaration[A] extends Tree[A]
sealed trait TopLevelDeclaration[A] extends Declaration[A]
sealed trait LocalDeclaration[A] extends Declaration[A]

final case class Let[A](name: String, binding: Expr[A], meta: A) extends TopLevelDeclaration[A] with LocalDeclaration[A]

// final case class Def(name: String, params: Seq[Param], body: Expr) extends TopLevelDeclaration

sealed trait Name

case object NoName extends Name
case class LocalName(name: String) extends Name
case class FullName(pkg: Seq[String], name: String) extends Name

sealed trait Type

object Type {
  val Int = TypeConstructor("Int", Seq.empty)
  val Long = TypeConstructor("Long", Seq.empty)
  val Float = TypeConstructor("Float", Seq.empty)
  val Double = TypeConstructor("Double", Seq.empty)
  val Boolean = TypeConstructor("Boolean", Seq.empty)
  val Char = TypeConstructor("Char", Seq.empty)
  val String = TypeConstructor("String", Seq.empty)
  val Module = TypeConstructor("Module", Seq.empty)
  def Function(from: Type, to: Type) = TypeConstructor("->", Seq(from, to))
}

case class TypeVariable(id: Int, name: String) extends Type

object TypeVariable {
  var nextVariableId = 0
  def apply(): TypeVariable = {
    val tyVar = TypeVariable(nextVariableId, "A" + nextVariableId)
    nextVariableId += 1
    tyVar
  }
}

case class TypeConstructor(name: String, typeParams: Seq[Type]) extends Type

case class NameWithType(name: Name, typ: Type)
