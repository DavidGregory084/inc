package inc.parser

import fastparse._, ScalaWhitespace._
import inc.common._
import java.lang.{ Boolean, Character, Double, Float, Integer, Long, String }
import scala.{ Either, Right, Int, Some, None, StringContext }
import scala.collection.immutable.{ List, Map, Seq }
import scala.Predef.augmentString

class ExprParser(typeEnv: Map[String, Type]) {
  import Parser._

  def ifExpr[_: P] = P(
    Index ~
    "if" ~/ expression ~
      "then" ~ expression ~
      "else" ~ expression ~ Index
  ).map {
    case (from, cond, thenExpr, elseExpr, to) =>
      If(cond, thenExpr, elseExpr, Pos(from, to))
  }

  def constrPattern[_: P]: P[Pattern[Pos]] = P(
    Index ~
      (identifier ~ "@").? ~ reference ~ "{" ~/ fieldPattern.rep(sep = comma./) ~ "}" ~
      Index
  ).map {
    case (from, alias, ref, patterns, to) =>
      ConstrPattern(ref.fullName, alias, patterns.toList, Pos(from, to))
  }

  def fieldPattern[_: P]: P[FieldPattern[Pos]] = P(
    Index ~
      identifier ~ (":" ~/ pattern).? ~
      Index
  ).map {
    case (from, field, pattern, to) =>
      FieldPattern(field, pattern, Pos(from, to))
  }

  def idPattern[_: P]: P[Pattern[Pos]] = P(
    Index ~ identifier ~ Index
  ).map {
    case (from, name, to) =>
      IdentPattern(name, Pos(from, to))
  }

  def pattern[_: P]: P[Pattern[Pos]] = P( constrPattern | idPattern )

  def matchCase[_: P] = P(
    Index ~
      "case" ~/ pattern ~ "->" ~ expression ~ Index
  ).map {
    case (from, pat, resultExpr, to) =>
      MatchCase(pat, resultExpr, Pos(from, to))
  }

  def matchExpr[_: P] = P(
    Index ~
      "match" ~/ expression ~
      "with" ~ inBraces(matchCase.rep(min = 1, sep = maybeSemi)) ~ Index
  ).map {
    case (from, expr, cases, to) =>
      Match(expr, cases.toList, Pos(from, to))
  }

  def constructorParam[_: P] = P(
    Index ~ identifier ~ ":" ~ typeExpr ~ Index
  ).map {
    case (from, name, ascribedAs, to) =>
      Param(name, Some(TypeScheme(List.empty, ascribedAs)), Pos(from, to))
  }

  def lambdaParam[_: P] = P(
    Index ~ identifier ~ (":" ~ typeExpr).? ~ Index
  ).map {
    case (from, name, ascribedAs, to) =>
      Param(name, ascribedAs.map(TypeScheme(List.empty, _)), Pos(from, to))
  }

  def lambda[_: P] = P(
    Index ~
    (inParens(lambdaParam.rep(sep = comma./)) | lambdaParam.map(Seq(_))) ~ "->" ~/
      expression ~
      Index
  ).map {
    case (from, params, expr, to) =>
      Lambda(params.toList, expr, Pos(from, to))
  }

  def funTypeExpr[_: P]: P[Type] = P(
    Index ~ (inParens(typeExpr.rep(sep = comma./)) | primaryTypeExpr.map(Seq(_))) ~ "->" ~/ typeExpr ~ Index
  ).map {
    case (from, paramTyps, returnTyp, to) =>
      Type.Function(paramTyps.toList, returnTyp, Pos(from, to))
  }

  def primaryTypeExpr[_: P]: P[Type] = P(
    Index ~ identifier.rep(min = 1, sep = ".") ~ Index ~ inSquareBraces(typeExpr.rep(min = 1, sep = comma./)).? ~ Index
  ).map {
    case (from, id, endTyCon, Some(params), endTyApp) =>
      val kind = Kind.Function(params.length)
      val name = id.mkString(".")

      val tp = typeEnv
        .getOrElse(name, TypeConstructor(name, kind))
        .withPos(Pos(from, endTyCon))

      TypeApply(tp, params.toList, KindVariable(), Pos(from, endTyApp))

    case (from, id, _, None, to) =>
      val name = id.mkString(".")

      typeEnv
        .getOrElse(name, TypeConstructor(name, KindVariable()))
        .withPos(Pos(from, to))
  }

  def typeExpr[_: P]: P[Type] =
    P( NoCut(funTypeExpr) | primaryTypeExpr | inParens(typeExpr) )

  def application[_: P]: P[Expr[Pos] => Expr[Pos]] = P(
    inParens(expression.rep(sep = comma./)) ~ Index
  ).map {
    case (args, to) =>
      fn => Apply(fn, args.toList, Pos(fn.meta.from, to))
  }

  // NoCut allows us to backtrack out of a nullary lambda into a unit literal, and from an if statement into an identifier starting with "if"
  def primaryExpr[_:P] = P( (NoCut(lambda) | NoCut(ifExpr) | matchExpr | literal | reference | inParens(expression)) ~ application.rep ).map {
    case (expr, applications) =>
      applications.foldLeft(expr) { case (expr, app) => app(expr) }
  }

  def expression[_: P]: P[Expr[Pos]] = P(
    Index ~
      primaryExpr ~ (":" ~ typeExpr).? ~
      Index
  ).map {
    case (from, expr, Some(ascribedAs), to) =>
      Ascription(expr, TypeScheme(List.empty, ascribedAs), Pos(from, to))
    case (_, expr, None, _) =>
      expr
  }
}

object Parser {
  def ReservedWords[_: P] = P(
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
  def nonZeroWs[_: P] = P(CharsWhile(Character.isWhitespace, 1))

  // Separators
  def maybeSemi[_: P] = P(";".?)
  def comma[_: P] = P(",")

  // Literals
  def zero[_: P] = P("0".!)
  def oneToNine[_: P] = P(CharIn("1-9").!)
  def zeroToNine[_: P] = P(CharIn("0-9").!)
  def digits[_: P] = P(CharsWhileIn("0-9", 0).!)
  def digitsLeadingOneToNine[_: P] = P((oneToNine ~~ digits) map { case (first, rest) => first + rest })

  def literalBoolean[_: P] = P(Index ~ StringIn("true", "false").! ~ Index).map {
    case (from, b, to) =>
      LiteralBoolean(Boolean.parseBoolean(b), Pos(from, to))
  }

  val disallowedChars = "\\\n\r"
  val charDisallowedChars = "\'" + disallowedChars
  val stringDisallowedChars = "\"" + disallowedChars

  def literalChar[_: P] = P(Index ~ "'" ~~/ CharPred(c => !charDisallowedChars.contains(c)).! ~~ "'" ~ Index).map {
    case (from, s, to) => LiteralChar(s(0), Pos(from, to))
  }

  def literalString[_: P] = P(
    Index ~
      "\"" ~~/ CharsWhile(c => !stringDisallowedChars.contains(c), 0).! ~~ "\"" ~
      Index
  ).map {
    case (from, s, to) =>
      LiteralString(s, Pos(from, to))
  }

  def literalIntegral[_: P] = P(Index ~ (zero | digitsLeadingOneToNine) ~~ CharIn("lL").?.! ~ Index).map {
    case (from, num, suffix, to) =>
      if (suffix.isEmpty)
        LiteralInt(Integer.parseInt(num), Pos(from, to))
      else
        LiteralLong(Long.parseLong(num), Pos(from, to))
  }

  def exponentPart[_: P] = P(
    (CharIn("eE").! ~~/ CharIn("+\\-").?.! ~~ digits).?
  ).map {
    case Some((exponentIndicator, sign, digits)) =>
      exponentIndicator + sign + digits.mkString
    case None =>
      ""
  }

  def literalFloatingPoint[_: P] = P(
    Index ~
      (zero | digitsLeadingOneToNine) ~~ "." ~~/ digits ~~ exponentPart ~~ CharIn("dDfF").?.! ~
      Index
  ).map {
    case (from, wholeNumberPart, fractionPart, exponentPart, "", to) =>
      LiteralDouble(Double.parseDouble(wholeNumberPart + "." + fractionPart + exponentPart), Pos(from, to))
    case (from, wholeNumberPart, fractionPart, exponentPart, suffix, to) if suffix.toUpperCase == "D" =>
      LiteralDouble(Double.parseDouble(wholeNumberPart + "." + fractionPart + exponentPart), Pos(from, to))
    case (from, wholeNumberPart, fractionPart, exponentPart, suffix, to) if suffix.toUpperCase == "F" =>
      LiteralFloat(Float.parseFloat(wholeNumberPart + "." + fractionPart + exponentPart), Pos(from, to))
  }

  def literalUnit[_: P] = P(Index ~ "()" ~ Index).map {
    case (from, to) =>
      LiteralUnit(Pos(from, to))
  }

  def literal[_: P] =
    literalFloatingPoint |
      literalIntegral |
      literalChar |
      literalString |
      literalBoolean |
      literalUnit

  // Identifiers
  def identifier[_: P] = !ReservedWords ~~ P((CharPred(Character.isJavaIdentifierStart).! ~~ CharsWhile(Character.isJavaIdentifierPart, 0).!).map {
    case (first, rest) => first + rest
  })

  // Blocks
  def inBraces[_: P, A](p: => P[A]) = P("{" ~/ p ~ "}")
  def inParens[_: P, A](p: => P[A]) = P("(" ~/ p ~ ")")
  def inSquareBraces[_: P, A](p: => P[A]) = P("[" ~/ p ~ "]")

  def reference[_: P] = P(Index ~ (identifier.rep(sep = "/", min = 0) ~ ".").? ~ identifier ~ Index).map {
    case (from, Some(mod), id, to) =>
      Reference(mod.toList, id, Pos(from, to))
    case (from, None, id, to) =>
      Reference(List.empty, id, Pos(from, to))
  }

  def letDeclaration[_: P] = {
    val exprParser = new ExprParser(typeEnv = Map.empty)

    P(
      Index ~ "let" ~/ identifier ~ "=" ~
        (inBraces(exprParser.expression) | exprParser.expression) ~
        Index
    ).map {
      case (from, name, expr, to) =>
        Let(name, expr, Pos(from, to))
    }
  }

  def dataConstructor[_: P](parentType: TypeScheme, typeEnv: Map[String, Type]) ={
    val exprParser = new ExprParser(typeEnv)

    P(
      Index ~ "case" ~/ identifier ~ inParens(exprParser.constructorParam.rep(sep = comma./)) ~ Index
    ).map {
      case (from, name, params, to) =>
        DataConstructor(name, params.toList, parentType, Pos(from, to))
    }
  }

  def typeParam[_: P] = P( Index ~ identifier ~ Index ).map {
    case (from, nm, to) =>
      (nm, NamedTypeVariable(nm, kind = KindVariable(), Pos(from, to)))
  }

  def typeParams[_: P] = P(
    "[" ~ typeParam.rep(min = 1, sep = comma./) ~ "]"
  )

  def dataDeclaration[_: P] = P(
    (Index ~ "data" ~/ identifier ~ Index ~ typeParams.? ~ Index).flatMap {
        case (from, name, endTyCon, Some(mapping), endTyApp) =>
          val tparams = mapping.map { case (_, typ) => typ }.toList
          val kind = Parameterized(tparams.map(_.kind), Atomic)
          val tyCon = TypeConstructor(name, kind, Pos(from, endTyCon))
          val tyApp = TypeApply(tyCon, tparams, Atomic, Pos(from, endTyApp))
          val typ = TypeScheme(tparams, tyApp)
          val typeEnv = mapping.toMap.updated(name, tyCon)
          inBraces(dataConstructor(typ, typeEnv).rep(sep = maybeSemi./)).map { cases =>
            (from, name, tparams, cases)
          }
        case (from, name, _, None, to) =>
          val tyCon = TypeConstructor(name, Atomic, Pos(from, to))
          val typ = TypeScheme(List.empty, tyCon)
          val typeEnv = Map.empty.updated(name, tyCon)
          inBraces(dataConstructor(typ, typeEnv).rep(sep = maybeSemi./)).map { cases =>
            (from, name, List.empty, cases)
          }
      } ~ Index
  ).map {
    case (from, name, tparams, cases, to) =>
      Data(name, tparams.toList, cases.toList, Pos(from, to))
  }

  def decl[_: P] = P(letDeclaration | dataDeclaration)

  def importedSymbols[_: P] = P( inBraces(identifier.rep(min = 1, sep = comma./)) | identifier.map(Seq(_)) )

  def imports[_: P] = P {
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

  def bracesBlock[_: P] = P(inBraces(imports.rep(sep = maybeSemi) ~ maybeSemi ~ decl.rep(sep = maybeSemi)))

  def module[_: P] = P {
    Index ~
    "module" ~/ identifier.rep(min = 1, sep = "/") ~
      bracesBlock ~ Index ~
      End
  }.map {
    case (from, moduleName, (imports, decls), to) =>
      val pos = Pos(from, to)
      if (moduleName.length > 1)
        Module(moduleName.dropRight(1).toList, moduleName.lastOption.getOrElse(""), imports.toList, decls.toList, pos)
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
