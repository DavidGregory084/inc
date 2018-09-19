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

  val literalBoolean = P("true" | "false").!.map { b => LiteralBoolean(Boolean.parseBoolean(b), ()) }

  val disallowedChars = "\\\n\r"
  val charDisallowedChars = "\'" + disallowedChars
  val stringDisallowedChars = "\"" + disallowedChars

  val literalChar = P("'" ~/ CharPred(c => !charDisallowedChars.contains(c)).! ~ "'" ).map(s => LiteralChar(s(0), ()))

  val literalString = P("\"" ~/ CharsWhile(c => !stringDisallowedChars.contains(c), min = 0).! ~ "\"").map(s => LiteralString(s, ()))

  val literalIntegral = P((zero | digitsLeadingOneToNine) ~ CharIn("lL").?.!).map {
    case (num, suffix) =>
      if (suffix.isEmpty)
        LiteralInt(Integer.parseInt(num), ())
      else
        LiteralLong(Long.parseLong(num), ())
  }

  val literalFloatingPoint = P((zero | digitsLeadingOneToNine) ~ "." ~/ digits ~ (CharIn("dD") | CharIn("fF")).?.!).map {
    case (wholeNumberPart, fractionPart, "") =>
      LiteralDouble(Double.parseDouble(wholeNumberPart + "." + fractionPart), ())
    case (wholeNumberPart, fractionPart, suffix) if suffix.toUpperCase == "D" =>
      LiteralDouble(Double.parseDouble(wholeNumberPart + "." + fractionPart), ())
    case (wholeNumberPart, fractionPart, suffix) if suffix.toUpperCase == "F" =>
      LiteralFloat(Float.parseFloat(wholeNumberPart + "." + fractionPart), ())
  }

  val literalUnit = P( "()" ).map { _ =>
    LiteralUnit(())
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

  val reference = identifier.map(id => Reference(id, ()))

  val ifExpr = P(
    "if" ~/ allWs ~ expression ~ allWs ~
      "then" ~ allWs ~ expression ~ allWs ~
      "else" ~ allWs ~ expression ~ ws
  ).map {
    case (cond, thenExpr, elseExpr) =>
      If(cond, thenExpr, elseExpr, ())
  }

  val lambda = P(
    identifier ~ allWs ~ "->" ~/ allWs ~
      expression
  ).map {
    case (variable, expression) =>
      Lambda(variable, expression, ())
  }

  val expression: Parser[Expr[Unit]] = literal | ifExpr | lambda | reference

  val letDeclaration = P(
    "let" ~/ allWs ~ identifier ~ allWs ~ "=" ~ allWs ~
      (inBraces(expression) | expression)
      ~ ws
  ).map {
    case (name, expr) =>
      Let(name, expr, ())
  }

  val decl = P(letDeclaration)

  val imports = P {
    "import" ~/ ws ~ identifier.rep(min = 1, sep = ".") ~ ("." ~ inBraces(identifier.rep(min = 1, sep = comma))).?
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
    "module" ~/ ws ~ (identifier.rep(min = 1, sep = ".")) ~ allWs ~
      bracesBlock ~
      allWs ~ End
  }.map {
    case (moduleName, (imports, decls)) =>
      if (moduleName.length > 1)
        Module(moduleName.dropRight(1).toList, moduleName.lastOption.getOrElse(""), imports.toList, decls.toList, ())
      else
        Module(List.empty, moduleName.headOption.getOrElse(""), imports.toList, decls.toList, ())
  }

  def parse(fileContents: String): Either[List[ParserError], Module[Unit]] = {
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
