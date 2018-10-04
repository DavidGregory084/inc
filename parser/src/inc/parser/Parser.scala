package inc.parser

import java.lang.{ Boolean, Double, Float, Long }

import inc.common._

import fastparse.all._

object Parser {
  val ReservedWords = P(
    "module" |
    "import" |
    "let" |
    "if" |
    "then" |
    "else"
  )

  // Whitespace
  val ws = P(CharsWhile(_ == ' ', min = 0))
  val allWs = P(CharsWhile(Character.isWhitespace, min = 0))

  // Separators
  val maybeSemi = P(";".? ~ allWs)
  val comma = P("," ~ allWs)

  // Literals
  val zero = P("0".!)
  val oneToNine = P(CharIn('1' to '9'))
  val zeroToNine = P(CharIn('0' to '9'))
  val digits = P(zeroToNine.rep.!)
  val digitsLeadingOneToNine = P((oneToNine.rep(exactly = 1).! ~ zeroToNine.rep.!).map { case (first, rest) => first + rest })

  val literalBoolean = P(Index ~ ("true" | "false").! ~ Index).map {
    case (from, b, to) =>
      LiteralBoolean(Boolean.parseBoolean(b), Pos(from, to))
  }

  val disallowedChars = "\\\n\r"
  val charDisallowedChars = "\'" + disallowedChars
  val stringDisallowedChars = "\"" + disallowedChars

  val literalChar = P(Index ~ "'" ~/ CharPred(c => !charDisallowedChars.contains(c)).! ~ "'" ~ Index).map {
    case (from, s, to) => LiteralChar(s(0), Pos(from, to))
  }

  val literalString = P(
    Index ~
      "\"" ~/ CharsWhile(c => !stringDisallowedChars.contains(c), min = 0).! ~ "\"" ~
      Index
  ).map {
    case (from, s, to) =>
      LiteralString(s, Pos(from, to))
  }

  val literalIntegral = P(Index ~ (zero | digitsLeadingOneToNine) ~ CharIn("lL").?.! ~ Index).map {
    case (from, num, suffix, to) =>
      if (suffix.isEmpty)
        LiteralInt(Integer.parseInt(num), Pos(from, to))
      else
        LiteralLong(Long.parseLong(num), Pos(from, to))
  }

  val literalFloatingPoint = P(
    Index ~
      (zero | digitsLeadingOneToNine) ~ "." ~/ digits ~ (CharIn("dD") | CharIn("fF")).?.! ~
      Index
  ).map {
    case (from, wholeNumberPart, fractionPart, "", to) =>
      LiteralDouble(Double.parseDouble(wholeNumberPart + "." + fractionPart), Pos(from, to))
    case (from, wholeNumberPart, fractionPart, suffix, to) if suffix.toUpperCase == "D" =>
      LiteralDouble(Double.parseDouble(wholeNumberPart + "." + fractionPart), Pos(from, to))
    case (from, wholeNumberPart, fractionPart, suffix, to) if suffix.toUpperCase == "F" =>
      LiteralFloat(Float.parseFloat(wholeNumberPart + "." + fractionPart), Pos(from, to))
  }

  val literalUnit = P(Index ~ "()" ~ Index).map {
    case (from, to) =>
      LiteralUnit(Pos(from, to))
  }

  val literal =
    literalFloatingPoint |
      literalIntegral |
      literalChar |
      literalString |
      literalBoolean |
      literalUnit

  // Identifiers
  val identifier = !ReservedWords ~ P((CharPred(Character.isJavaIdentifierStart).! ~ CharsWhile(Character.isJavaIdentifierPart).rep.!).map {
    case (first, rest) => first + rest
  })

  // Blocks
  def inBraces[A](p: Parser[A]) = P("{" ~/ allWs ~ p ~ allWs ~ "}")
  def inParens[A](p: Parser[A]) = P("(" ~/ allWs ~ p ~ allWs ~ ")")

  val reference = P(Index ~ identifier ~ Index).map {
    case (from, id, to) =>
      Reference(id, Pos(from, to))
  }

  val ifExpr = P(
    Index ~
    "if" ~/ allWs ~ expression ~ allWs ~
      "then" ~ allWs ~ expression ~ allWs ~
      "else" ~ allWs ~ expression ~ Index ~ ws
  ).map {
    case (from, cond, thenExpr, elseExpr, to) =>
      If(cond, thenExpr, elseExpr, Pos(from, to))
  }

  val lambda = P(
    Index ~
    (inParens(identifier.rep(sep = comma.~/)) | identifier.map(Seq(_))) ~ allWs ~ "->" ~/ allWs ~
      expression ~
      Index
  ).map {
    case (from, variables, expr, to) =>
      Lambda(variables.toList, expr, Pos(from, to))
  }

  val application: Parser[Expr[Pos] => Expr[Pos]] = P(
    Index ~ inParens(expression.rep(sep = comma.~/)) ~ Index
  ).map {
    case (from, args, to) =>
      fn => Apply(fn, args.toList, Pos(from, to))
  }

  // NoCut allows us to backtrack out of a nullary lambda into a unit literal
  val expression: Parser[Expr[Pos]] = P( (NoCut(lambda) | literal | ifExpr | reference) ~ application.rep ).map {
    case (expr, applications) =>
      applications.foldLeft(expr) {
        case (expr, app) =>
          app(expr)
      }
  }

  val letDeclaration = P(
    Index ~ "let" ~/ allWs ~ identifier ~ allWs ~ "=" ~ allWs ~
      (inBraces(expression) | expression) ~
      Index ~ ws
  ).map {
    case (from, name, expr, to) =>
      Let(name, expr, Pos(from, to))
  }

  val decl = P(letDeclaration)

  val imports = P {
    "import" ~/ ws ~ identifier.rep(min = 1, sep = ".") ~ ("." ~ inBraces(identifier.rep(min = 1, sep = comma.~/))).?
  }.map {
    case (ident, Some(symbols)) =>
      if (ident.length > 1)
        ImportSymbols(ident.init.toList, ident.last, symbols.toList)
      else
        ImportSymbols(List.empty, ident.head, symbols.toList)
    case (ident, None) =>
      if (ident.length > 1)
        ImportModule(ident.init.toList, ident.last)
      else
        ImportModule(List.empty, ident.head)
  }

  val bracesBlock = P(inBraces(imports.rep(sep = maybeSemi) ~ maybeSemi ~ allWs ~ decl.rep(sep = maybeSemi)))

  val module = P {
    allWs ~ Index ~
    "module" ~/ ws ~ (identifier.rep(min = 1, sep = ".")) ~ allWs ~
      bracesBlock ~ Index ~
      allWs ~ End
  }.map {
    case (from, moduleName, (imports, decls), to) =>
      val pos = Pos(from, to)
      if (moduleName.length > 1)
        Module(moduleName.dropRight(1).toList, moduleName.lastOption.getOrElse(""), imports.toList, decls.toList, pos)
      else
        Module(List.empty, moduleName.headOption.getOrElse(""), imports.toList, decls.toList, pos)
  }

  def parse(fileContents: String): Either[List[ParserError], Module[Pos]] = {
    module.parse(fileContents) match {
      case Parsed.Success(mod, _) =>
        Right(mod)
      case Parsed.Failure(_, index, extra) =>
        val input = extra.input
        val expected = extra.traced.expected
        val errorMessage = input.repr.errorMessage(input, expected, index)
        ParserError.singleton(index, errorMessage)
    }
  }
}
