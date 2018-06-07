package inc.parser

import java.lang.{ Boolean, Long }

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
  val digits = P((oneToNine.rep(exactly = 1).! ~ zeroToNine.rep.!).map { case (first, rest) => first + rest })

  val literalBoolean = P("true" | "false").!.map { b => LiteralBoolean(Boolean.parseBoolean(b)) }

  val disallowedChars = "\\\n\r"
  val charDisallowedChars = "\'" + disallowedChars
  val stringDisallowedChars = "\"" + disallowedChars

  val literalChar = P("'" ~/ CharPred(c => !charDisallowedChars.contains(c)).! ~ "'" ).map(s => LiteralChar(s(0)))

  val literalString = P("\"" ~/ CharsWhile(c => !stringDisallowedChars.contains(c), min = 0).! ~ "\"").map(LiteralString)

  val literalNum = P(((zero | digits) ~ CharIn("lL").?.!).map {
    case (num, suffix) =>
      if (suffix.isEmpty)
        LiteralInt(Integer.parseInt(num))
      else
        LiteralLong(Long.parseLong(num))
  })

  val literal =
    literalNum |
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
        Module(Some(moduleName.dropRight(1).mkString(".")), moduleName.lastOption.getOrElse(""), decls)
      else
        Module(None, moduleName.headOption.getOrElse(""), decls)
  }

  def parse(fileContents: String): Module = {
    module.parse(fileContents) match {
      case Parsed.Success(mod, _) =>
        mod
      case f: Parsed.Failure =>
        println(f.msg)
        println(f.extra.traced)
        throw new Exception("Parsing failed")
    }
  }
}
