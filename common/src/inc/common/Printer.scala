package inc.common

import org.typelevel.paiges._

object Printer {

  def print(subst: Map[TypeVariable, Type]): String = {
    subst.map {
      case (tyVar, typ) =>
        print(tyVar) + " |-> " + print(typ)
    }.mkString(", ")
  }

  def print(typ: Type): String = typ match {
    case TypeVariable(i) =>
      "T" + i
    case TypeConstructor(nm, tyParams) =>
      if (tyParams.isEmpty)
        nm
      else if (nm == "->") {
        val args =
          if (tyParams.length == 2)
            print(tyParams.head)
          else
            tyParams.init.map(print).mkString("(", ", ", ")")

        args + " -> " + print(tyParams.last)

      } else
        nm + tyParams.map(print).mkString("[", ", ", "]")
  }

  def print(typ: TypeScheme): String = {
    if (typ.bound.isEmpty)
      print(typ.typ)
    else
      typ.bound.map(print).mkString("[", ", ", "]") + "{ " + print(typ.typ) + " }"
  }

  def print[A](e: Expr[A]): Doc = e match {
    case LiteralInt(i, _) =>
      Doc.str(i)
    case LiteralLong(l, _) =>
      Doc.str(l + 'L')
    case LiteralFloat(f, _) =>
      Doc.str(f + 'F')
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
      Doc.text("if") & print(c) /
        (Doc.text("then") & print(t)).nested(2) /
        (Doc.text("else") & print(e)).nested(2)
    case Lambda(vs, b, _) =>
      val args =
        if (vs.length == 1)
          Doc.text(vs.head)
        else
          Doc.intercalate(Doc.char(',') + Doc.space, vs.map(Doc.text)).tightBracketBy(Doc.char('('), Doc.char(')'))

      args & Doc.text("->") & print(b).nested(2)

    case Apply(fn, args, _) =>
      val prefix = print(fn) + Doc.char('(')
      val suffix = Doc.char(')')
      val argsList = Doc.intercalate(Doc.char(',') + Doc.line, args.map(print(_)))
      argsList.tightBracketBy(prefix, suffix)
  }

  def print[A](decl: TopLevelDeclaration[A]): Doc = decl match {
    case Let(name, binding, _) =>
      Doc.text("let") & Doc.text(name) & Doc.char('=') & print(binding).nested(2)
  }

  def print[A](mod: Module[A]): Doc = {
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

    val decls = Doc.intercalate(Doc.char(';') + Doc.line, declarations.map(print(_)))

    val body =
      if (imps.isEmpty) decls
      else imps + Doc.char(';') + Doc.line + decls

    body.bracketBy(prefix, suffix, indent = 2)
  }
}
