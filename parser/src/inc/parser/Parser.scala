package inc.parser

import java.lang.{ Boolean, Double, Float, Long }

import inc.common._

import fastparse.all._

object Parser {
  // Whitespace
  val ws = P(CharsWhile(_ == ' ', min = 0))
  val allWs = P(CharsWhile(Character.isWhitespace, min = 0))

  // Separators
  val maybeSemi = P(";".? ~ allWs)

  // Literals
  val zero = P("0".!)
  val oneToNine = P(CharIn('1' to '9'))
  val zeroToNine = P(CharIn('0' to '9'))
  val digits = P(zeroToNine.rep.!)
  val digitsLeadingOneToNine = P((oneToNine.rep(exactly = 1).! ~ zeroToNine.rep.!).map { case (first, rest) => first + rest })

  val literalBoolean = P("true" | "false").!.map { b => LiteralBoolean(Boolean.parseBoolean(b)) }

  val disallowedChars = "\\\n\r"
  val charDisallowedChars = "\'" + disallowedChars
  val stringDisallowedChars = "\"" + disallowedChars

  val literalChar = P("'" ~/ CharPred(c => !charDisallowedChars.contains(c)).! ~ "'" ).map(s => LiteralChar(s(0)))

  val literalString = P("\"" ~/ CharsWhile(c => !stringDisallowedChars.contains(c), min = 0).! ~ "\"").map(LiteralString)

  val literalIntegral = P((zero | digitsLeadingOneToNine) ~ CharIn("lL").?.!).map {
    case (num, suffix) =>
      if (suffix.isEmpty)
        LiteralInt(Integer.parseInt(num))
      else
        LiteralLong(Long.parseLong(num))
  }

  val literalFloatingPoint = P((zero | digitsLeadingOneToNine) ~ "." ~/ digits ~ (CharIn("dD") | CharIn("fF")).?.!).map {
    case (wholeNumberPart, fractionPart, "") =>
      LiteralDouble(Double.parseDouble(wholeNumberPart + "." + fractionPart))
    case (wholeNumberPart, fractionPart, suffix) if suffix.toUpperCase == "D" =>
      LiteralDouble(Double.parseDouble(wholeNumberPart + "." + fractionPart))
    case (wholeNumberPart, fractionPart, suffix) if suffix.toUpperCase == "F" =>
      LiteralFloat(Float.parseFloat(wholeNumberPart + "." + fractionPart))
  }

  val literal =
    literalFloatingPoint |
      literalIntegral |
      literalChar |
      literalString |
      literalBoolean

  // Identifiers
  val identifier = P((CharPred(Character.isJavaIdentifierStart).! ~ CharsWhile(Character.isJavaIdentifierPart).rep.!).map {
    case (first, rest) => first + rest
  })

  // Blocks
  def inBraces[A](p: Parser[A]) = P("{" ~/ allWs ~ p ~ allWs ~ "}")

  val letDeclaration = P(
    "let" ~/ allWs ~ identifier ~ allWs ~ "=" ~ allWs ~
      (inBraces(literal) | literal)
      ~ ws
  ).map(Let.tupled)

  val decl = P(letDeclaration)

  val bracesBlock = P(inBraces(decl.rep(sep = maybeSemi)))

  val module = P {
    "module" ~/ ws ~ (identifier.rep(min = 1, sep = ".")) ~ allWs ~
      bracesBlock ~
      allWs ~ End
  }.map {
    case (moduleName, decls) =>
      if (moduleName.length > 1)
        Module(moduleName.dropRight(1), moduleName.lastOption.getOrElse(""), decls)
      else
        Module(Seq.empty, moduleName.headOption.getOrElse(""), decls)
  }

  def parse(fileContents: String): Either[ParserError, Module] = {
    module.parse(fileContents) match {
      case Parsed.Success(mod, _) =>
        Right(mod)
      case f: Parsed.Failure =>
        val traced = f.extra.traced
        val expected = traced.expected
        val index = traced.index
        val input = traced.input
        val errorMessage = input.repr.errorMessage(input, expected, index)
        Left { ParserError(index, errorMessage) }
    }
  }
}
