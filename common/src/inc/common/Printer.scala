package inc.common

import java.lang.{ String, System }
import org.typelevel.paiges._
import scala.{ Int, StringContext, Some, None }
import scala.collection.immutable.Map
import scala.Predef.{ augmentString, wrapRefArray }

object Printer {
  case class SourceContext(consoleWidth: Int, fileName: String, source: String)

  def withMargin(marginWidth: Int, lineNumber: Int, line: String) = {
    val margin = White(s"%${marginWidth}d".format(lineNumber) + '|')
    margin + line
  }

  def withSourceContext(context: SourceContext)(msg: String, pos: Pos, highlight: Style): String = {
    if (pos.isEmpty || context.source.isEmpty)
      NL + msg
    else {
      val blue = Style.Ansi.Fg.Blue
      val header = blue.start + context.fileName + blue.end

      val before = context.source.substring(0, pos.from)
      val highlighted = context.source.substring(pos.from, pos.to)
      val after = context.source.substring(pos.to, context.source.length)

      val dropBefore = before.count(c => c == '\r' || c == '\n')
      val dropAfter = after.count(c => c == '\r' || c == '\n')

      val highlightedString = {
        val highlightedLines = highlighted.split("\\r?\\n")
        val firstLine = highlightedLines.head
        val remaining = highlightedLines.tail.map(highlight.start + _)
        val fullyHighlighted = (firstLine +: remaining).mkString(System.lineSeparator)
        before + highlight.start + fullyHighlighted + highlight.end + after
      }

      val allLines = highlightedString.split("\\r?\\n")

      val marginWidth = String.valueOf(allLines.length).length + 1

      val highlightedLines = allLines
        .zipWithIndex
        .map { case (line, idx) => withMargin(marginWidth, idx + 1, line) }
        .drop(dropBefore)
        .dropRight(dropAfter)

      NL + header + NL + highlightedLines.mkString(System.lineSeparator) + NL + msg
    }
  }

  def print(constraint: Constraint): Doc = constraint match {
    case Equal(l, r, _) =>
      print(l) + Doc.text(" \u2261 ") + print(r)
  }

  def print(constraint: KindConstraint): Doc = constraint match {
    case EqualKind(l, r, _) =>
      print(l) + Doc.text(" \u2261 ") + print(r)
  }

  def print[K, V](subst: Map[K, V])(printKey: K => Doc, printValue: V => Doc): Doc = {
    Doc.intercalate(Doc.char(',') + Doc.space, subst.map {
      case (k, v) =>
        printKey(k) + Doc.text(" |-> ") + printValue(v)
    })
  }

  def print(typ: Type): Doc = typ match {
    case NamedTypeVariable(n, _, _) =>
      Doc.text(n)
    case InferredTypeVariable(i, _, _) =>
      Doc.text("T" + i.toString)
    case TypeConstructor(nm, _, _) =>
      Doc.text(nm)
    case TypeApply(typ, params, _, _) if params.isEmpty =>
      print(typ)
    case Type.Function(params) =>
      val withParens = params.headOption match {
        case Some(ta @ Type.Function(_)) =>
          print(ta).tightBracketBy(Doc.char('('), Doc.char(')'))
        case Some(other) =>
          print(other)
        case None =>
          Doc.empty
      }

      val paramDocs =
        if (params.length == 2)
          withParens
        else {
          Doc.intercalate(Doc.char(',') + Doc.space, params.init.map(print))
            .tightBracketBy(Doc.char('('), Doc.char(')'))
        }

      paramDocs & Doc.text("->") & print(params.last)

    case TypeApply(typ, params, _, _) =>
      print(typ) + Doc.intercalate(Doc.char(',') + Doc.space, params.map(print))
        .tightBracketBy(Doc.char('['), Doc.char(']'))
  }

  def print(kind: Kind): Doc = kind match {
    case Atomic =>
      Doc.char('*')
    case KindVariable(id) =>
      Doc.text("K"+ id.toString)
    case Parameterized(params, result) =>
      val withParens = params.headOption match {
        case Some(p @ Parameterized(_, _)) =>
          print(p).tightBracketBy(Doc.char('('), Doc.char(')'))
        case Some(other) =>
          print(other)
        case None =>
          Doc.empty
      }

      val paramDocs =
        if (params.length == 1)
          withParens
        else
          Doc.intercalate(Doc.char(',') + Doc.space, params.map(print))
            .tightBracketBy(Doc.char('('), Doc.char(')'))

      paramDocs & Doc.text("->") & print(result)
  }

  def print(typ: TypeScheme): Doc = {
    if (typ.bound.isEmpty)
      print(typ.typ)
    else {
      val bound = Doc.intercalate(Doc.char(',') + Doc.space, typ.bound.map(print))
      bound.tightBracketBy(Doc.char('['), Doc.char(']')) & print(typ.typ).bracketBy(Doc.char('{'), Doc.char('}'))
    }
  }

  def print[A](p: Param[A]): Doc = {
    Doc.text(p.name) +
      p.ascribedAs.map(asc => Doc.char(':') & print(asc)).getOrElse(Doc.empty)
  }

  def print[A](p: FieldPattern[A]): Doc = p match {
    case FieldPattern(field, pattern, _) =>
      Doc.text(field) + pattern.map(p => Doc.char(':') & print(p)).getOrElse(Doc.empty)
  }

  def print[A](p: Pattern[A]): Doc = p match {
    case IdentPattern(name, _) =>
      Doc.text(name)
    case ConstrPattern(name, alias, patterns, _) =>
      alias.map(a => Doc.text(a) & Doc.char('@') + Doc.space).getOrElse(Doc.empty) +
        Doc.text(name) & Doc.intercalate(
          Doc.comma + Doc.space,
          patterns.map(print(_))
        ).bracketBy(Doc.char('{'), Doc.char('}'))
  }

  def print[A](e: Expr[A]): Doc = e match {
    case LiteralInt(i, _) =>
      Doc.str(i)
    case LiteralLong(l, _) =>
      Doc.str(s"${l}L")
    case LiteralFloat(f, _) =>
      Doc.str(s"${f}F")
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
    case Reference(mod, name, _) =>
      if (mod.isEmpty)
        Doc.text(name)
      else
        Doc.intercalate(Doc.char('/'), mod.map(Doc.text)) +
          Doc.char('.') + Doc.text(name)
    case If(c, t, e, _) =>
      Doc.text("if") & print(c) /
        (Doc.text("then") & print(t)).nested(2) /
        (Doc.text("else") & print(e)).nested(2)
    case Lambda(params, b, _) =>
      val args =
        if (params.length == 1)
          params.head.ascribedAs.map { _ =>
            print(params.head).tightBracketBy(Doc.char('('), Doc.char(')'))
          }.getOrElse(print(params.head))
        else
          Doc.intercalate(
            Doc.char(',') + Doc.space,
            params.map(print(_))
          ).tightBracketBy(Doc.char('('), Doc.char(')'))

      args & Doc.text("->") & print(b).nested(2)

    case Apply(fn, args, _) =>
      val prefix = print(fn) + Doc.char('(')
      val suffix = Doc.char(')')
      val argsList = Doc.intercalate(Doc.char(',') + Doc.line, args.map(print(_)))
      argsList.tightBracketBy(prefix, suffix)

    case Ascription(expr, ascribedAs, _) =>
      Doc.char('(') + print(expr) + Doc.char(')') + Doc.text(": ") + print(ascribedAs)

    case Match(matchExpr, cases, _) =>
      Doc.text("match") & print(matchExpr) & Doc.text("with") & Doc.intercalate(
        Doc.char(';') + Doc.line,
        cases.map {
          case MatchCase(pattern, resultExpr, _) =>
            Doc.text("case") & print(pattern) & Doc.text("->") & print(resultExpr)
      }).bracketBy(Doc.char('{'), Doc.char('}'))
  }

  def print[A](data: DataConstructor[A]): Doc = data match {
    case DataConstructor(name, params, _, _) =>
      val prefix = Doc.text("case") & Doc.text(name) + Doc.char('(')
      val suffix = Doc.char(')')
      val argsList = Doc.intercalate(Doc.char(',') + Doc.space, params.map(print(_)))
      argsList.tightBracketBy(prefix, suffix)
  }

  def print[A](decl: TopLevelDeclaration[A]): Doc = decl match {
    case Let(name, binding, _) =>
      Doc.text("let") & Doc.text(name) & Doc.char('=') & print(binding).nested(2)
    case Data(name, tparams, cases, _) =>
      val typeParams =
        if (tparams.isEmpty)
          Doc.empty
        else
          Doc.intercalate(
            Doc.text(", "),
            tparams.map(print)).tightBracketBy(Doc.char('['), Doc.char(']'))

      val prefix = Doc.text("data") & Doc.text(name) + typeParams & Doc.char('{')
      val suffix = Doc.char('}')
      val body = Doc.intercalate(Doc.char(';') + Doc.line, cases.map(print(_)))
      body.bracketBy(prefix, suffix, indent = 2)
  }

  def print[A](mod: Module[A]): Doc = {
    val Module(pkg, name, imports, declarations @ _, _) = mod
    val prefix = Doc.text("module") & Doc.text((pkg :+ name).mkString("/")) & Doc.char('{')
    val suffix = Doc.char('}')

    val imps = Doc.intercalate(Doc.char(';') + Doc.line, imports.map {
      case ImportModule(pkg, name, _) =>
        Doc.text("import") & Doc.text((pkg :+ name).mkString("/"))
      case ImportSymbols(pkg, name, syms, _) =>
        val impPrefix = Doc.text("import") & Doc.text((pkg :+ name).mkString("/")) + Doc.char('.') + Doc.char('{')
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
