package inc.common

sealed trait Tree extends Product with Serializable

final case class Module(pkg: Option[String], name: String, declarations: Seq[Declaration]) extends Tree

sealed trait Expr extends Tree

final case class LiteralInt(i: Int) extends Expr
final case class LiteralLong(l: Long) extends Expr
final case class LiteralBoolean(b: Boolean) extends Expr
final case class LiteralChar(c: Char) extends Expr
final case class LiteralString(s: String) extends Expr

sealed trait Declaration extends Tree

final case class Let(name: String, binding: Expr) extends Declaration
