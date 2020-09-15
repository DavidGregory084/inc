package inc.common

import org.typelevel.paiges._
import scala.{ =:=, Boolean, Some, None, StringContext }

object Printer {
  private val White = Style.Ansi.Fg.White
  private val Yellow = Style.Ansi.Fg.Yellow
  private val Blue = Style.Ansi.Fg.Blue

  def annotate(doc: Doc, typ: Type, annotated: Boolean): Doc = {
    if (annotated) {
      val leftParen = Doc.char('(').style(Yellow)
      val rightParen = Doc.char(')').style(Yellow)
      val colon = Doc.char(':')
      val annotation = (colon & print(typ.kind)).style(Yellow)
      (doc + annotation).tightBracketBy(leftParen, rightParen)
    } else doc
  }

  def annotate[A](doc: Doc, tree: SyntaxTree[A], annotated: Boolean)
    (implicit eqv: A =:= Meta.Typed): Doc = {
    if (annotated) {
      val leftParen = Doc.char('(').style(Blue)
      val rightParen = Doc.char(')').style(Blue)
      val colon = Doc.char(':').style(Blue)
      val annotation = colon & print(tree.meta.typ, annotated)
      (doc + annotation).tightBracketBy(leftParen, rightParen)
    } else doc
  }

  def print(constraint: TypeConstraint): Doc = constraint match {
    case EqualType(l, r, _) =>
      print(l, true) + Doc.text(" ~ ") + print(r, true)
  }

  def print(constraint: KindConstraint): Doc = constraint match {
    case EqualKind(l, r, _) =>
      print(l) + Doc.text(" ~ ") + print(r)
  }

  def printTypeSubst(subst: Substitution[TypeVariable, Type]): Doc = {
    Doc.intercalate(Doc.hardLine, subst.subst.map {
      case (tv, typ) =>
        print(tv, true) + Doc.text(" \u21a6 ") + print(typ, true)
    })
  }

  def printKindSubst(subst: Substitution[KindVariable, Kind]): Doc = {
    Doc.intercalate(Doc.hardLine, subst.subst.map {
      case (kv, kind) =>
        print(kv) + Doc.text(" \u21a6 ") + print(kind)
    })
  }

  def print(typ: Type, annotated: Boolean): Doc = {
    val typDoc = typ match {
      case NamedTypeVariable(n, _) =>
        Doc.text(n).style(Blue)
      case InferredTypeVariable(i, _) =>
        Doc.text("T" + i.toString).style(Blue)
      case TypeConstructor(nm, _) =>
        Doc.text(nm).style(Blue)
      case TypeApply(typ, params, _) if params.isEmpty =>
        print(typ, annotated)
      case Type.Function(params) =>
        val withParens = params.headOption match {
          case Some(ta @ Type.Function(_)) =>
            print(ta, annotated).tightBracketBy(Doc.char('('), Doc.char(')'))
          case Some(other) =>
            print(other, annotated)
          case None =>
            Doc.empty
        }

        val paramDocs =
          if (params.length == 2)
            withParens
          else {
            Doc.intercalate(
              Doc.char(',') + Doc.space,
              params.init.map(print(_, annotated))
            ).tightBracketBy(Doc.char('('), Doc.char(')'))
          }

        paramDocs & Doc.text("->") & print(params.last, annotated)

      case TypeApply(typ, params, _) =>
        print(typ, annotated) + Doc.intercalate(
          Doc.char(',') + Doc.space,
          params.map(print(_, annotated))
        ).tightBracketBy(Doc.char('['), Doc.char(']'))

      case ErrorType =>
        Doc.text("<error>")
    }

    annotate(typDoc, typ, annotated)
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

  def print(typ: TypeScheme, annotated: Boolean): Doc = {
    if (typ.bound.isEmpty)
      print(typ.typ, annotated)
    else {
      val bound = Doc.intercalate(
        Doc.char(',') + Doc.space,
        typ.bound.map(print(_, annotated))
      ).tightBracketBy(Doc.char('['), Doc.char(']'))

      val scheme = print(typ.typ, annotated)
        .bracketBy(Doc.char('{'), Doc.char('}'))

      bound & scheme
    }
  }

  def print[A](expr: TypeExpr[A], annotated: Boolean)
    (implicit eqv: A =:= Meta.Typed): Doc = {
    val tyExpDoc = expr match {
      case TypeApplyExpr(typ, args, _) if args.isEmpty =>
        print(typ, annotated)

      case TypeExpr.Function(args) =>
        val withParens = args.headOption match {
          case Some(fn @ TypeExpr.Function(_)) =>
            print(fn, annotated)
              .tightBracketBy(Doc.char('('), Doc.char(')'))
          case Some(other) =>
            print(other, annotated)
          case None =>
            Doc.empty
        }

        val argDocs =
          if (args.length == 2)
            withParens
          else
            Doc.intercalate(
              Doc.char(',') + Doc.space,
              args.init.map(print(_, annotated))
            ).tightBracketBy(Doc.char('('), Doc.char(')'))

        argDocs & Doc.text("->") & print(args.last, annotated)

      case TypeApplyExpr(typ, args, _) =>
        print(typ, annotated) + Doc.intercalate(
          Doc.char(',') + Doc.space,
          args.map(print(_, annotated))
        ).tightBracketBy(Doc.char('['), Doc.char(']'))

      case TypeConstructorExpr(name, _) =>
        Doc.text(name)
    }

    annotate(tyExpDoc, expr, annotated)
  }

  def print[A](p: Param[A], annotated: Boolean)
    (implicit eqv: A =:= Meta.Typed): Doc = {
    val paramDoc = Doc.text(p.name) +
      p.ascribedAs.map { asc =>
        Doc.char(':') & print(asc, annotated)
      }.getOrElse(Doc.empty)

    annotate(paramDoc, p, annotated)
  }

  def print[A](p: FieldPattern[A], annotated: Boolean)
    (implicit eqv: A =:= Meta.Typed): Doc = p match {
    case FieldPattern(field, pattern, _) =>
      val patDoc = Doc.text(field) + pattern.map { p =>
        Doc.char(':') & print(p, annotated)
      }.getOrElse(Doc.empty)

      annotate(patDoc, p, annotated)
  }

  def print[A](p: Pattern[A], annotated: Boolean)
    (implicit eqv: A =:= Meta.Typed): Doc = {
    val patDoc = p match {
      case IdentPattern(name, _) =>
        Doc.text(name)
      case ConstrPattern(name, alias, patterns, _) =>
        alias.map(a => Doc.text(a) & Doc.char('@') + Doc.space).getOrElse(Doc.empty) +
          Doc.text(name) & Doc.intercalate(
            Doc.comma + Doc.space,
            patterns.map(print(_, annotated))
          ).bracketBy(Doc.char('{'), Doc.char('}'))
    }

    annotate(patDoc, p, annotated)
  }

  def print[A](e: Expr[A], annotated: Boolean)
    (implicit eqv: A =:= Meta.Typed): Doc = {
    val exprDoc = e match {
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
        Doc.text("if") & print(c, annotated) /
          (Doc.text("then") & print(t, annotated)).nested(2) /
          (Doc.text("else") & print(e, annotated)).nested(2)
      case Lambda(params, b, _) =>
        val args =
          if (params.length == 1)
            params.head.ascribedAs.map { _ =>
              print(params.head, annotated).tightBracketBy(Doc.char('('), Doc.char(')'))
            }.getOrElse(print(params.head, annotated))
          else
            Doc.intercalate(
              Doc.char(',') + Doc.space,
              params.map(print(_, annotated))
            ).tightBracketBy(Doc.char('('), Doc.char(')'))

        args & Doc.text("->") & print(b, annotated).nested(2)

      case Apply(fn, args, _) =>
        val prefix = print(fn, annotated) + Doc.char('(')
        val suffix = Doc.char(')')
        val argsList = Doc.intercalate(Doc.char(',') + Doc.line, args.map(print(_, annotated)))
        argsList.tightBracketBy(prefix, suffix)

      case Ascription(expr, ascribedAs, _) =>
        val ascribedExpr = print(expr, annotated)
          .tightBracketBy(Doc.char('('), Doc.char(')'))

        ascribedExpr + Doc.text(": ") + print(ascribedAs, annotated)

      case Match(matchExpr, cases, _) =>
        Doc.text("match") & print(matchExpr, annotated) & Doc.text("with") & Doc.intercalate(
          Doc.hardLine,
          cases.map {
            case MatchCase(pattern, resultExpr, _) =>
              Doc.text("case") & print(pattern, annotated) &
                Doc.text("->") &
                  print(resultExpr, annotated)
        }).bracketBy(Doc.char('{'), Doc.char('}'))
    }

    annotate(exprDoc, e, annotated)
  }

  def print[A](constr: DataConstructor[A], annotated: Boolean)
    (implicit eqv: A =:= Meta.Typed): Doc = constr match {

    case DataConstructor(name, params, _) =>
      val prefix = Doc.text("case") & Doc.text(name) + Doc.char('(')
      val suffix = Doc.char(')')

      val constrDoc = Doc.intercalate(
        Doc.char(',') + Doc.space,
        params.map(print(_, annotated))
      ).tightBracketBy(prefix, suffix)

      annotate(constrDoc, constr, annotated)
  }

  def print[A](decl: TopLevelDeclaration[A], annotated: Boolean)
    (implicit eqv: A =:= Meta.Typed): Doc = decl match {
    case Let(name, binding, _) =>
      val letDoc = Doc.text("let") & Doc.text(name) & Doc.char('=') &
        print(binding, annotated).nested(2)

      annotate(letDoc, decl, annotated)

    case Data(name, tparams, cases, _) =>
      val typeParams =
        if (tparams.isEmpty)
          Doc.empty
        else
          Doc.intercalate(
            Doc.text(", "),
            tparams.map(print(_, annotated))
          ).tightBracketBy(Doc.char('['), Doc.char(']'))

      val prefix = Doc.text("data") & Doc.text(name) + typeParams & Doc.char('{')
      val suffix = Doc.char('}')

      val casesDoc =
        Doc.intercalate(
          Doc.hardLine,
          cases.map(print(_, annotated))
        ).indent(2)

      val dataDoc =
        prefix + Doc.hardLine +
          casesDoc + Doc.hardLine +
          suffix

      annotate(dataDoc, decl, annotated)
  }

  def print[A](mod: Module[A], annotated: Boolean)
    (implicit eqv: A =:= Meta.Typed): Doc = {
    val Module(pkg, name, imports, declarations @ _, _) = mod
    val prefix = Doc.text("module") & Doc.text((pkg :+ name).mkString("/")) & Doc.char('{')
    val suffix = Doc.char('}')

    val imps = Doc.intercalate(Doc.hardLine, imports.map {
      case ImportModule(pkg, name, _) =>
        Doc.text("import") & Doc.text((pkg :+ name).mkString("/"))
      case ImportSymbols(pkg, name, syms, _) =>
        val impPrefix = Doc.text("import") & Doc.text((pkg :+ name).mkString("/")) + Doc.char('.') + Doc.char('{')
        val impSuffix = Doc.char('}')
        val impBody = Doc.intercalate(Doc.comma + Doc.line, syms.map(Doc.text))
        impBody.bracketBy(impPrefix, impSuffix)
    })

    val decls = Doc.intercalate(
      Doc.hardLine,
      declarations.map(print(_, annotated))
    )

    val body =
      if (imps.isEmpty) decls
      else imps + Doc.lineOr(Doc.char(';') + Doc.space) + decls

    val bullet = Doc.char('*').style(White)

    val key =
      if (annotated)
        Doc.text("Key: ").style(White) + Doc.hardLine +
          (bullet & Doc.text("Unstyled:") & Doc.text("original code")).indent(2) + Doc.hardLine +
          (bullet & (Doc.text("Blue:") & Doc.text("inferred types")).style(Blue)).indent(2) + Doc.hardLine +
          (bullet & (Doc.text("Yellow:") & Doc.text("inferred kinds")).style(Yellow)).indent(2) + (Doc.hardLine * 2)
      else
        Doc.empty

    key + prefix + Doc.hardLine +
      body.indent(2) + Doc.hardLine +
      suffix
  }
}
