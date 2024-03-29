package inc.parser

import fastparse._
import inc.common._

import java.lang.Boolean
import java.lang.Character
import java.lang.Double
import java.lang.Float
import java.lang.Integer
import java.lang.Long
import java.lang.String
import scala.Either
import scala.Int
import scala.None
import scala.Predef.augmentString
import scala.Right
import scala.Some
import scala.StringContext
import scala.collection.immutable.List
import scala.collection.immutable.Seq

import ScalaWhitespace._

object Parser {
  def ReservedWords[Ctx: P] = P(
    StringIn(
      "module",
      "import",
      "let",
      "if",
      "then",
      "else",
      "case",
      "data",
      "match",
      "with"
    ) ~~ nonZeroWs
  )

  // Whitespace
  def nonZeroWs[Ctx: P] = P(CharsWhile(Character.isWhitespace, 1))

  // Separators
  def maybeSemi[Ctx: P] = P(";".?)
  def comma[Ctx: P]     = P(",")

  // Literals
  def zero[Ctx: P]       = P("0".!)
  def oneToNine[Ctx: P]  = P(CharIn("1-9").!)
  def zeroToNine[Ctx: P] = P(CharIn("0-9").!)
  def digits[Ctx: P]     = P(CharsWhileIn("0-9", 0).!)
  def digitsLeadingOneToNine[Ctx: P] = P((oneToNine ~~ digits) map { case (first, rest) =>
    first + rest
  })

  def literalBoolean[Ctx: P] = P(Index ~ StringIn("true", "false").! ~ Index).map {
    case (from, b, to) =>
      LiteralBoolean(Boolean.parseBoolean(b), Pos(from, to))
  }

  val disallowedChars       = "\\\n\r"
  val charDisallowedChars   = "\'" + disallowedChars
  val stringDisallowedChars = "\"" + disallowedChars

  def literalChar[Ctx: P] =
    P(Index ~ "'" ~~/ CharPred(c => !charDisallowedChars.contains(c)).! ~~ "'" ~ Index).map {
      case (from, s, to) => LiteralChar(s(0), Pos(from, to))
    }

  def literalString[Ctx: P] = P(
    Index ~
      "\"" ~~/ CharsWhile(c => !stringDisallowedChars.contains(c), 0).! ~~ "\"" ~
      Index
  ).map { case (from, s, to) =>
    LiteralString(s, Pos(from, to))
  }

  def literalIntegral[Ctx: P] =
    P(Index ~ (zero | digitsLeadingOneToNine) ~~ CharIn("lL").?.! ~ Index).map {
      case (from, num, suffix, to) =>
        if (suffix.isEmpty)
          LiteralInt(Integer.parseInt(num), Pos(from, to))
        else
          LiteralLong(Long.parseLong(num), Pos(from, to))
    }

  def exponentPart[Ctx: P] = P(
    (CharIn("eE").! ~~/ CharIn("+\\-").?.! ~~ digits).?
  ).map {
    case Some((exponentIndicator, sign, digits)) =>
      exponentIndicator + sign + digits.mkString
    case None =>
      ""
  }

  def literalFloatingPoint[Ctx: P] = P(
    Index ~
      (zero | digitsLeadingOneToNine) ~~ "." ~~/ digits ~~ exponentPart ~~ CharIn("dDfF").?.! ~
      Index
  ).map {
    case (from, wholeNumberPart, fractionPart, exponentPart, "", to) =>
      LiteralDouble(
        Double.parseDouble(wholeNumberPart + "." + fractionPart + exponentPart),
        Pos(from, to)
      )
    case (from, wholeNumberPart, fractionPart, exponentPart, suffix, to)
        if suffix.toUpperCase == "D" =>
      LiteralDouble(
        Double.parseDouble(wholeNumberPart + "." + fractionPart + exponentPart),
        Pos(from, to)
      )
    case (from, wholeNumberPart, fractionPart, exponentPart, suffix, to)
        if suffix.toUpperCase == "F" =>
      LiteralFloat(
        Float.parseFloat(wholeNumberPart + "." + fractionPart + exponentPart),
        Pos(from, to)
      )
  }

  def literalUnit[Ctx: P] = P(Index ~ "()" ~ Index).map { case (from, to) =>
    LiteralUnit(Pos(from, to))
  }

  def literal[Ctx: P] =
    literalFloatingPoint |
      literalIntegral |
      literalChar |
      literalString |
      literalBoolean |
      literalUnit

  // Identifiers
  def identifier[Ctx: P] = !ReservedWords ~~ P(
    (CharPred(Character.isJavaIdentifierStart).! ~~ CharsWhile(Character.isJavaIdentifierPart, 0).!)
      .map { case (first, rest) =>
        first + rest
      }
  )

  // Blocks
  def inBraces[Ctx: P, A](p: => P[A])       = P("{" ~/ p ~ "}")
  def inParens[Ctx: P, A](p: => P[A])       = P("(" ~/ p ~ ")")
  def inSquareBraces[Ctx: P, A](p: => P[A]) = P("[" ~/ p ~ "]")

  def reference[Ctx: P] =
    P(Index ~ (identifier.rep(sep = "/", min = 0) ~ ".").? ~ identifier ~ Index).map {
      case (from, Some(mod), id, to) =>
        Reference(mod.toList, id, Pos(from, to))
      case (from, None, id, to) =>
        Reference(List.empty, id, Pos(from, to))
    }

  // Expressions
  def ifExpr[Ctx: P] = P(
    Index ~
      "if" ~/ expression ~
      "then" ~ expression ~
      "else" ~ expression ~ Index
  ).map { case (from, cond, thenExpr, elseExpr, to) =>
    If(cond, thenExpr, elseExpr, Pos(from, to))
  }

  def constrPattern[Ctx: P]: P[Pattern[Pos]] = P(
    Index ~
      (identifier ~ "@").? ~ reference ~ "{" ~/ fieldPattern.rep(sep = comma./) ~ "}" ~
      Index
  ).map { case (from, alias, ref, patterns, to) =>
    ConstrPattern(ref.fullName, alias, patterns.toList, Pos(from, to))
  }

  def fieldPattern[Ctx: P]: P[FieldPattern[Pos]] = P(
    Index ~
      identifier ~ (":" ~/ pattern).? ~
      Index
  ).map { case (from, field, pattern, to) =>
    FieldPattern(field, pattern, Pos(from, to))
  }

  def idPattern[Ctx: P]: P[Pattern[Pos]] = P(
    Index ~ identifier ~ Index
  ).map { case (from, name, to) =>
    IdentPattern(name, Pos(from, to))
  }

  def pattern[Ctx: P]: P[Pattern[Pos]] = P(constrPattern | idPattern)

  def matchCase[Ctx: P] = P(
    Index ~
      "case" ~/ pattern ~ "->" ~ expression ~ Index
  ).map { case (from, pat, resultExpr, to) =>
    MatchCase(pat, resultExpr, Pos(from, to))
  }

  def matchExpr[Ctx: P] = P(
    Index ~
      "match" ~/ expression ~
      "with" ~ inBraces(matchCase.rep(min = 1, sep = maybeSemi)) ~ Index
  ).map { case (from, expr, cases, to) =>
    Match(expr, cases.toList, Pos(from, to))
  }

  def constructorParam[Ctx: P] = P(
    Index ~ identifier ~ ":" ~ typeExpr ~ Index
  ).map { case (from, name, ascribedAs, to) =>
    Param(name, Some(ascribedAs), Pos(from, to))
  }

  def lambdaParam[Ctx: P] = P(
    Index ~ identifier ~ (":" ~ typeExpr).? ~ Index
  ).map { case (from, name, ascribedAs, to) =>
    Param(name, ascribedAs, Pos(from, to))
  }

  def lambda[Ctx: P] = P(
    Index ~
      (inParens(lambdaParam.rep(sep = comma./)) | lambdaParam.map(Seq(_))) ~ "->" ~/
      expression ~
      Index
  ).map { case (from, params, expr, to) =>
    Lambda(params.toList, expr, Pos(from, to))
  }

  def typeVarExpr[Ctx: P]: P[TypeConstructorExpr[Pos]] = P(
    Index ~ identifier ~ Index
  ).map { case (from, id, to) =>
    TypeConstructorExpr(id, Pos(from, to))
  }

  def funTypeExpr[Ctx: P]: P[TypeExpr[Pos]] = P(
    Index ~ (inParens(typeExpr.rep(sep = comma./)) | primaryTypeExpr.map(
      Seq(_)
    )) ~ "->" ~/ typeExpr ~ Index
  ).map { case (from, paramTyps, returnTyp, to) =>
    TypeApplyExpr(
      TypeConstructorExpr("->", Pos(from, to)),
      (paramTyps :+ returnTyp).toList,
      Pos(from, to)
    )
  }

  def primaryTypeExpr[Ctx: P]: P[TypeExpr[Pos]] = P(
    Index ~ (identifier.rep(sep = "/", min = 0) ~ ".").? ~ identifier ~ Index ~ inSquareBraces(
      typeExpr.rep(min = 1, sep = comma./)
    ).? ~ Index
  ).map {
    case (from, mod, name, _, None, to) =>
      val fullName = if (mod.isEmpty) name else mod.mkString("/") + "." + name
      TypeConstructorExpr(fullName, Pos(from, to))
    case (from, mod, name, endTyCon, Some(params), endTyApp) =>
      val fullName = if (mod.isEmpty) name else mod.mkString("/") + "." + name
      TypeApplyExpr(
        TypeConstructorExpr(fullName, Pos(from, endTyCon)),
        params.toList,
        Pos(from, endTyApp)
      )
  }

  def typeExpr[Ctx: P]: P[TypeExpr[Pos]] =
    P(NoCut(funTypeExpr) | primaryTypeExpr | inParens(typeExpr))

  def application[Ctx: P]: P[Expr[Pos] => Expr[Pos]] = P(
    inParens(expression.rep(sep = comma./)) ~ Index
  ).map { case (args, to) =>
    fn => Apply(fn, args.toList, Pos(fn.meta.from, to))
  }

  // NoCut allows us to backtrack out of a nullary lambda into a unit literal, and from an if statement into an identifier starting with "if"
  def primaryExpr[Ctx: P] = P(
    (NoCut(lambda) | NoCut(ifExpr) | matchExpr | literal | reference | inParens(
      expression
    )) ~ application.rep
  ).map { case (expr, applications) =>
    applications.foldLeft(expr) { case (expr, app) => app(expr) }
  }

  def expression[Ctx: P]: P[Expr[Pos]] = P(
    Index ~
      primaryExpr ~ (":" ~ typeExpr).? ~
      Index
  ).map {
    case (from, expr, Some(ascribedAs), to) =>
      Ascription(expr, ascribedAs, Pos(from, to))
    case (_, expr, None, _) =>
      expr
  }

  // Top level declarations
  def letDeclaration[Ctx: P] = {
    P(
      Index ~ "let" ~/ identifier ~ "=" ~
        (inBraces(expression) | expression) ~
        Index
    ).map { case (from, name, expr, to) =>
      Let(name, expr, Pos(from, to))
    }
  }

  def dataConstructor[Ctx: P] = {
    P(
      Index ~ "case" ~/ identifier ~ inParens(constructorParam.rep(sep = comma./)) ~ Index
    ).map { case (from, name, params, to) =>
      DataConstructor(name, params.toList, Pos(from, to))
    }
  }

  def typeParams[Ctx: P] = P(
    inSquareBraces(typeVarExpr.rep(min = 1, sep = comma./))
  )

  def dataDeclaration[Ctx: P] = P(
    Index ~ "data" ~/ identifier ~ typeParams.? ~
      inBraces(dataConstructor.rep(sep = maybeSemi./)) ~
      Index
  ).map { case (from, name, tparams, cases, to) =>
    Data(name, tparams.map(_.toList).getOrElse(List.empty), cases.toList, Pos(from, to))
  }

  def decl[Ctx: P] = P(letDeclaration | dataDeclaration)

  def importedSymbols[Ctx: P] = P(
    inBraces(identifier.rep(min = 1, sep = comma./)) | identifier.map(Seq(_))
  )

  def imports[Ctx: P] = P {
    Index ~
      "import" ~/ identifier.rep(min = 1, sep = "/") ~
      ("." ~ importedSymbols).? ~
      Index
  }.map {
    case (from, ident, Some(symbols), to) =>
      if (ident.length > 1)
        ImportSymbols(ident.init.toList, ident.last, symbols.toList, Pos(from, to))
      else
        ImportSymbols(List.empty, ident.head, symbols.toList, Pos(from, to))
    case (from, ident, None, to) =>
      if (ident.length > 1)
        ImportModule(ident.init.toList, ident.last, Pos(from, to))
      else
        ImportModule(List.empty, ident.head, Pos(from, to))
  }

  def bracesBlock[Ctx: P] = P(
    inBraces(imports.rep(sep = maybeSemi) ~ maybeSemi ~ decl.rep(sep = maybeSemi))
  )

  def module[Ctx: P] = P {
    Index ~
      "module" ~/ identifier.rep(min = 1, sep = "/") ~
      bracesBlock ~ Index ~
      End
  }.map { case (from, moduleName, (imports, decls), to) =>
    val pos = Pos(from, to)
    if (moduleName.length > 1)
      Module(
        moduleName.dropRight(1).toList,
        moduleName.lastOption.getOrElse(""),
        imports.toList,
        decls.toList,
        pos
      )
    else
      Module(List.empty, moduleName.headOption.getOrElse(""), imports.toList, decls.toList, pos)
  }

  def parse(fileContents: String): Either[List[ParserError], Module[Pos]] = {
    fastparse.parse(fileContents, module(_)) match {
      case Parsed.Success(mod, _) =>
        Right(mod)
      case f @ Parsed.Failure(_, _, _) =>
        val errorMessage = f.trace().longMsg
        ParserError.singleton(errorMessage)
    }
  }
}
