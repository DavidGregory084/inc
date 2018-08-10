package inc.common

import org.typelevel.paiges._

object Printer {
  def print[A](mod: Module[A]) = {
    val Module(pkg, name, imports, declarations @ _, _) = mod
    val prefix = Doc.text("module ") + Doc.text((pkg :+ name).mkString(".")) + Doc.space + Doc.char('{')
    val suffix = Doc.char('}')

    val imps = Doc.intercalate(Doc.char(';') + Doc.line, imports.map {
      case ImportModule(pkg, name) =>
        Doc.text("import ") + Doc.text((pkg :+ name).mkString("."))
      case ImportSymbols(pkg, name, syms) =>
        val impPrefix = Doc.text("import ") + Doc.text((pkg :+ name).mkString(".")) + Doc.char('.') + Doc.char('{')
        val impSuffix = Doc.char('}')
        val impBody = Doc.intercalate(Doc.comma + Doc.line, syms.map(Doc.text))
        impBody.bracketBy(impPrefix, impSuffix)
    })

    def expr(e: Expr[A]) = e match {
      case LiteralInt(i, _) =>
        Doc.str(i)
      case LiteralLong(l, _) =>
        Doc.str(l)
      case LiteralFloat(f, _) =>
        Doc.str(f)
      case LiteralDouble(d, _) =>
        Doc.str(d)
      case LiteralBoolean(b, _) =>
        Doc.str(b)
      case LiteralChar(c, _) =>
        Doc.char('\'') + Doc.str(c) + Doc.char('\'')
      case LiteralString(s, _) =>
        Doc.char('"') + Doc.str(s) + Doc.char('"')
      case LiteralUnit(_) =>
        Doc.text("()")
      case Reference(ref, _) =>
        Doc.text(ref)
    }

    val decls = Doc.intercalate(Doc.char(';') + Doc.line, declarations.map {
      case Let(name, binding, _) =>
        Doc.text("let ") + Doc.text(name) + Doc.space + Doc.char('=') + Doc.space + expr(binding)
    })

    val body =
      if (imps.isEmpty) decls
      else imps + Doc.lineBreak + decls

    body.bracketBy(prefix, suffix)
  }
}
