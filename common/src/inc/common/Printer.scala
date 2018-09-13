package inc.common

import org.typelevel.paiges._

object Printer {
  def print(typ: Type): String = typ match {
    case TypeVariable(i) =>
      "T" + i
    case TypeConstructor(nm, tyParams) =>
      if (tyParams.isEmpty)
        nm
      else if (nm == "->")
        print(tyParams.head) + " -> " + print(tyParams(1))
      else
        nm + tyParams.map(print).mkString("[", ", ", "]")
  }

  def print(typ: TypeScheme): String = {
    if (typ.bound.isEmpty)
      print(typ.typ)
    else
      typ.bound.map(print).mkString("[", ", ", "]") + "{ " + print(typ.typ) + " }"
  }

  def print[A](mod: Module[A]) = {
    val Module(pkg, name, imports, declarations @ _, _) = mod
    val prefix = Doc.text("module") & Doc.text((pkg :+ name).mkString(".")) & Doc.char('{')
    val suffix = Doc.char('}')

    val imps = Doc.intercalate(Doc.char(';') + Doc.line, imports.map {
      case ImportModule(pkg, name) =>
        Doc.text("import") & Doc.text((pkg :+ name).mkString("."))
      case ImportSymbols(pkg, name, syms) =>
        val impPrefix = Doc.text("import") & Doc.text((pkg :+ name).mkString(".")) + Doc.char('.') + Doc.char('{')
        val impSuffix = Doc.char('}')
        val impBody = Doc.intercalate(Doc.comma + Doc.line, syms.map(Doc.text))
        impBody.bracketBy(impPrefix, impSuffix)
    })

    def expr(e: Expr[A]): Doc = e match {
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
      case If(c, t, e, _) =>
        Doc.text("if") & expr(c) /
          (Doc.text("then") & expr(t)).nested(2) /
          (Doc.text("else") & expr(e)).nested(2)
      case Lambda(v, b, _) =>
        Doc.text(v) & Doc.text("->") & expr(b).nested(2)
    }

    val decls = Doc.intercalate(Doc.char(';') + Doc.line, declarations.map {
      case Let(name, binding, _) =>
        Doc.text("let") & Doc.text(name) & Doc.char('=') & expr(binding).nested(2)
    })

    val body =
      if (imps.isEmpty) decls
      else imps + Doc.lineBreak + decls

    body.bracketBy(prefix, suffix, indent = 2)
  }
}
