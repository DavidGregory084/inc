package inc.common

import cats.data.Chain
import java.lang.String
import org.typelevel.paiges._
import scala.{ Array, Option }
import scala.collection.immutable.Map
import scala.Predef.{ArrowAssoc, StringCanBuildFrom, augmentString, genericArrayOps}

object Printer {
  def regionWithMargin(inputLines: Array[String], pos: Pos) = {
    val numberOfLines = inputLines.length

    val (formattedLines, _) = inputLines.zipWithIndex.foldLeft(Chain.empty[String] -> 0) {
      case ((lines, charIdx), (line, lineIdx)) =>
        val nextCharIdx = charIdx + 1 + fansi.Str(line).length

        val nextLines =
          if (pos.from < charIdx || pos.to >= nextCharIdx)
            lines
          else {
            val lineNumber = lineIdx + 1
            val marginWidth = String.valueOf(numberOfLines).length + 1
            val margin = White(lineNumber.toString.padTo(marginWidth, ' ') + '|')
            lines :+ (margin + line)
          }

        (nextLines, nextCharIdx)
    }

    formattedLines.toList
  }

  def withSourceContext(header: Option[String], msg: String, pos: Pos, colour: fansi.Attrs, source: String) = {
    if (pos.isEmpty)
      NL + msg
    else {
      val highlighted = fansi.Str(source, errorMode = fansi.ErrorMode.Sanitize).overlay(colour, pos.from, pos.to)
      val highlightedLines = highlighted.render.split("\\r?\\n")
      val formattedLines = regionWithMargin(highlightedLines, pos).mkString(NL)
      val formattedHeader = header.map(h => Blue(h + ":") + NL).getOrElse("")
      NL + formattedHeader + formattedLines + (NL * 2) + msg
    }
  }


  def print(subst: Map[TypeVariable, Type]): String = {
    subst.map {
      case (tyVar, typ) =>
        print(tyVar) + White(" |-> ") + print(typ)
    }.mkString(", ")
  }

  def print(typ: Type): String = typ match {
    case TypeVariable(i) =>
      Yellow("T" + i)
    case TypeConstructor(nm, tyParams) =>
      if (tyParams.isEmpty)
        Yellow(nm)
      else if (nm == "->") {
        val args =
          if (tyParams.length == 2)
            print(tyParams.head)
          else
            tyParams.init.map(print).mkString(Yellow("("), ", ", Yellow(")"))

        args + Yellow(" -> ") + print(tyParams.last)

      } else {
        nm + tyParams.map(print).mkString("[", ", ", "]")
      }
  }

  def print(typ: TypeScheme): String = {
    if (typ.bound.isEmpty)
      print(typ.typ)
    else
      typ.bound.map(print).mkString("[", ", ", "]") + "{ " + print(typ.typ) + " }"
  }

  def print[A](e: Expr[A]): Doc = e match {
    case LiteralInt(i, _, _) =>
      Doc.str(i)
    case LiteralLong(l, _, _) =>
      Doc.str(l + "L")
    case LiteralFloat(f, _, _) =>
      Doc.str(f + "F")
    case LiteralDouble(d, _, _) =>
      Doc.str(d)
    case LiteralBoolean(b, _, _) =>
      Doc.str(b)
    case LiteralChar(c, _, _) =>
      Doc.char('\'') + Doc.str(c) + Doc.char('\'')
    case LiteralString(s, _, _) =>
      Doc.char('"') + Doc.str(s) + Doc.char('"')
    case LiteralUnit(_, _) =>
      Doc.text("()")
    case Reference(ref, _, _) =>
      Doc.text(ref)
    case If(c, t, e, _, _) =>
      Doc.text("if") & print(c) /
        (Doc.text("then") & print(t)).nested(2) /
        (Doc.text("else") & print(e)).nested(2)
    case Lambda(params, b, _, _) =>
      val args =
        if (params.length == 1)
          Doc.text(params.head.name)
        else
          Doc.intercalate(
            Doc.char(',') + Doc.space,
            params.map(p => Doc.text(p.name))
          ).tightBracketBy(Doc.char('('), Doc.char(')'))

      args & Doc.text("->") & print(b).nested(2)

    case Apply(fn, args, _, _) =>
      val prefix = print(fn) + Doc.char('(')
      val suffix = Doc.char(')')
      val argsList = Doc.intercalate(Doc.char(',') + Doc.line, args.map(print(_)))
      argsList.tightBracketBy(prefix, suffix)
  }

  def print[A](decl: TopLevelDeclaration[A]): Doc = decl match {
    case Let(name, binding, _, _) =>
      Doc.text("let") & Doc.text(name) & Doc.char('=') & print(binding).nested(2)
  }

  def print[A](mod: Module[A]): Doc = {
    val Module(pkg, name, imports, declarations @ _, _, _) = mod
    val prefix = Doc.text("module") & Doc.text((pkg :+ name).mkString(".")) & Doc.char('{')
    val suffix = Doc.char('}')

    val imps = Doc.intercalate(Doc.char(';') + Doc.line, imports.map {
      case ImportModule(pkg, name, _) =>
        Doc.text("import") & Doc.text((pkg :+ name).mkString("."))
      case ImportSymbols(pkg, name, syms, _) =>
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
