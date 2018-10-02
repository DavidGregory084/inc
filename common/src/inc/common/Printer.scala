package inc.common

import org.typelevel.paiges._
import org.typelevel.paiges.Document.ops._

object Printer {
  implicit val fansiStrDocument: Document[fansi.Str] =
    Document.instance(str => Doc.text(str.render))

  def Keyword(s: String) = fansi.Color.True(249, 38, 114)(s).doc
  def Constant[A](a: A) = fansi.Color.True(190, 132, 255)(a.toString).doc
  def String(s: String) = fansi.Color.True(230, 219, 116)('"' + s + '"').doc
  def Definition(s: String) = fansi.Color.True(166, 226, 46)(s).doc
  def Type(s: String) = fansi.Color.True(102, 217, 239)(s).doc

  def print(subst: Map[TypeVariable, Type]): String = {
    subst.map {
      case (tyVar, typ) => (
        fansi.Color.Red(print(tyVar)).doc &
          fansi.Color.Green(" |-> ").doc &
          fansi.Color.Green(print(typ)).doc
      ).render(80)
    }.mkString(", ")
  }

  def print(typ: Type): String = typ match {
    case TypeVariable(i) =>
      Type("T" + i).render(2)
    case TypeConstructor(nm, tyParams) =>
      if (tyParams.isEmpty)
        Definition(nm).render(nm.length)
      else if (nm == "->")
        print(tyParams.head) + Keyword(" -> ").render(4) + print(tyParams(1))
      else
        Definition(nm).render(nm.length) + tyParams.map(print).mkString("[", ", ", "]")
  }

  def print(typ: TypeScheme): String = {
    if (typ.bound.isEmpty)
      print(typ.typ)
    else
      typ.bound.map(print).mkString("[", ", ", "]") + "{ " + print(typ.typ) + " }"
  }

  def print[A](e: Expr[A]): Doc = e match {
    case LiteralInt(i, _) => Constant(i)
    case LiteralLong(l, _) => Constant(l)
    case LiteralFloat(f, _) => Constant(f)
    case LiteralDouble(d, _) => Constant(d)
    case LiteralBoolean(b, _) => Constant(b)
    case LiteralChar(c, _) => Constant(c)
    case LiteralString(s, _) => String(s)
    case LiteralUnit(_) => Constant("()")
    case Reference(ref, _) =>
      Doc.text(ref)
    case If(c, t, e, _) =>
      Keyword("if") & print(c) /
        (Keyword("then") & print(t)).nested(2) /
        (Keyword("else") & print(e)).nested(2)
    case Lambda(v, b, _) =>
      Doc.text(v) & Keyword("->") & print(b).nested(2)
    case Apply(fn, args, _) =>
      val prefix = print(fn) + Doc.char('(')
      val suffix = Doc.char(')')
      val argsList = Doc.intercalate(Doc.char(',') + Doc.line, args.map(print(_)))
      argsList.tightBracketBy(prefix, suffix)
  }

  def print[A](decl: TopLevelDeclaration[A]): Doc = decl match {
    case Let(name, binding, _) =>
      Keyword("let") & Definition(name) & Doc.char('=') & print(binding).nested(2)
  }

  def print[A](mod: Module[A]): Doc = {
    val Module(pkg, name, imports, declarations @ _, _) = mod
    val prefix = Keyword("module") &
      Definition((pkg :+ name).mkString(".")) &
      Doc.char('{')
    val suffix = Doc.char('}')

    val imps = Doc.intercalate(Doc.char(';') + Doc.line, imports.map {
      case ImportModule(pkg, name) =>
        Keyword("import") & Doc.text((pkg :+ name).mkString("."))
      case ImportSymbols(pkg, name, syms) =>
        val impPrefix = Keyword("import") & Doc.text((pkg :+ name).mkString(".")) + Doc.char('.') + Doc.char('{')
        val impSuffix = Doc.char('}')
        val impBody = Doc.intercalate(Doc.comma + Doc.line, syms.map(Doc.text))
        impBody.bracketBy(impPrefix, impSuffix)
    })

    val decls = Doc.intercalate(Doc.char(';') + Doc.line, declarations.map(print(_)))

    val body =
      if (imps.isEmpty) decls
      else imps + Doc.lineBreak + decls

    body.bracketBy(prefix, suffix, indent = 2)
  }
}
