package inc.common

import cats.data.Chain
import java.lang.String
import org.typelevel.paiges._
import scala.{ Array, Int, Option, StringContext }
import scala.collection.immutable.{ List, Map, Nil, :: }
import scala.Predef.{ augmentString, genericArrayOps }

object Printer {
  def regionWithMargin(plainInputLines: Array[String], highlightedSource: fansi.Str, pos: Pos) = {
    val numberOfLines = plainInputLines.length

    def go(plainInputLines: List[(String, Int)], highlightedSource: fansi.Str, charIdx: Int, output: Chain[fansi.Str]): fansi.Str =
      plainInputLines match {
        case Nil =>
          fansi.Str.join(output.toList: _*)
        case (plainLine, lineIdx) :: plainLines =>
          val nextCharIdx = charIdx + 1 + plainLine.length

          val (highlightedLine, highLightedLines) =
            if (plainLine.length >= highlightedSource.length)
              (highlightedSource, fansi.Str(""))
            else
              highlightedSource.splitAt(plainLine.length + 1)

          if (pos.from > nextCharIdx || pos.to < charIdx)
            go(plainLines, highLightedLines, nextCharIdx, output)
          else {
            val lineNumber = lineIdx + 1
            val marginWidth = String.valueOf(numberOfLines).length + 1
            val margin = fansi.Color.White(fansi.Str(s"%${marginWidth}d".format(lineNumber) + '|'))
            go(plainLines, highLightedLines, nextCharIdx, output :+ margin :+ highlightedLine)
          }
      }

    go(plainInputLines.toList.zipWithIndex, highlightedSource, 0, Chain.empty[fansi.Str])
  }

  def withSourceContext(header: Option[String], msg: String, pos: Pos, colour: fansi.Attrs, source: String) = {
    if (pos.isEmpty)
      NL + msg
    else {
      val highlightedSource = fansi.Str(source, errorMode = fansi.ErrorMode.Sanitize).overlay(colour, pos.from, pos.to)
      val plainSourceLines = source.split('\n')
      val formattedLines = regionWithMargin(plainSourceLines, highlightedSource, pos)
      val formattedHeader = header.map(h => Blue(h + ":") + NL).getOrElse("")
      NL + formattedHeader + formattedLines + NL + msg
    }
  }

  def print(constraint: Constraint): String = constraint match {
    case Equal(l, r, _) =>
      Printer.print(l) + " \u2261 " + Printer.print(r)
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
    case LiteralInt(i, _) =>
      Doc.str(i)
    case LiteralLong(l, _) =>
      Doc.str(l + "L")
    case LiteralFloat(f, _) =>
      Doc.str(f + "F")
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
    case Lambda(params, b, _) =>
      val args =
        if (params.length == 1)
          Doc.text(params.head.name)
        else
          Doc.intercalate(
            Doc.char(',') + Doc.space,
            params.map(p => Doc.text(p.name))
          ).tightBracketBy(Doc.char('('), Doc.char(')'))

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
