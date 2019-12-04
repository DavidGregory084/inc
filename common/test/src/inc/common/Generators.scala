package inc.common

import cats.data.StateT
import cats.implicits._
import org.scalatest._
import org.scalacheck._
import org.scalacheck.cats.implicits._

trait Generators { self: Matchers =>
  type Decl = TopLevelDeclaration[NamePosType]
  type Decls = List[TopLevelDeclaration[NamePosType]]

  val nameGen: Gen[String] =
    for {
      len <- Gen.choose(0, 5)
      first <- Gen.alphaChar
      rest <- Gen.resize(len, Gen.alphaNumStr)
      nm = (first +: rest).mkString
      if !List("module", "import", "let", "if", "then", "else").contains(nm)
    } yield nm

  // Don't generate negative numbers: the language doesn't have operators yet so no parsing of prefix negation
  val intGen: Gen[Expr[NamePosType]] =
    Arbitrary.arbitrary[Int]
      .filter(_ >= 0)
      .map(LiteralInt(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Int))))

  val longGen: Gen[Expr[NamePosType]] =
    Arbitrary.arbitrary[Long]
      .filter(_ >= 0L)
      .map(LiteralLong(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Long))))

  val fltGen: Gen[Expr[NamePosType]] =
    Arbitrary.arbitrary[Float]
      .filter(_ >= 0F)
      .map(LiteralFloat(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Float))))

  val dblGen: Gen[Expr[NamePosType]] =
    Arbitrary.arbitrary[Double]
      .filter(_ >= 0D)
      .map(LiteralDouble(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Double))))

  val boolGen: Gen[Expr[NamePosType]] =
    Arbitrary.arbitrary[Boolean]
      .map(LiteralBoolean(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Boolean))))

  val charGen: Gen[Expr[NamePosType]] =
    Gen.asciiPrintableChar.filterNot { chr =>
      // Parsing char and string escapes is not implemented yet
      chr == '\n' ||
        chr == '\r' ||
        chr == '\\' ||
        chr == '\''
    }.map(LiteralChar(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Char))))

  val strGen: Gen[Expr[NamePosType]] =
    Gen.asciiPrintableStr.filterNot { str =>
      // Parsing char and string escapes is not implemented yet
      str.contains('\n') ||
      str.contains('\r') ||
      str.contains('\\') ||
      str.contains('"')
    }.map(LiteralString(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.String))))

  val unitGen: Gen[Expr[NamePosType]] =
    Gen.const(LiteralUnit(NamePosType(NoName, Pos.Empty, TypeScheme(Type.Unit))))

  val literalGens: List[Gen[Expr[NamePosType]]] =
    List(intGen, longGen, fltGen, dblGen, boolGen, charGen, strGen, unitGen)

  def referenceGen(decls: Decls): Gen[Expr[NamePosType]] =
    Gen.oneOf(decls).flatMap {
      case Let(name, _, meta) =>
        Gen.const(Reference(List.empty, name, NamePosType(meta.name, Pos.Empty, meta.typ)))
      case Data(_, _, cases, _) =>
        Gen.oneOf(cases).map {
          case DataConstructor(caseName, _, _, caseMeta) =>
            Reference(List.empty, caseName, NamePosType(caseMeta.name, Pos.Empty, caseMeta.typ))
        }
    }

  def lambdaGen(decls: Decls): Gen[Expr[NamePosType]] =
    for {
      numArgs <- Gen.choose(1, 4)

      // Don't generate duplicate variable names
      pNms <- Gen.listOfN(numArgs, nameGen).suchThat(vs => vs.distinct.length == vs.length)

      pAscs <- Gen.listOfN(numArgs, Arbitrary.arbitrary[Boolean])

      pTps <- Gen.listOfN(numArgs, Gen.oneOf(
        TypeScheme(Type.Int),
        TypeScheme(Type.Long),
        TypeScheme(Type.Float),
        TypeScheme(Type.Double),
        TypeScheme(Type.Boolean),
        TypeScheme(Type.Char),
        TypeScheme(Type.String),
        TypeScheme(Type.Unit)))

      ps = pNms.lazyZip(pAscs).lazyZip(pTps).map {
        case (nm, ascribed, tp) =>
          Param(nm, if (ascribed) Some(tp) else None, NamePosType(LocalName(nm), Pos.Empty, tp))
      }

      body <- exprGen(
        // Trick to allow later generators to refer to params;
        // we need to filter out any existing declarations that
        // clash with our params, since params can shadow top level declarations
        decls.filterNot(d => ps.exists(_.name == d.name)) ++ ps.map { p =>
          Let(p.name, Reference(List.empty, p.name, p.meta), p.meta)
        }
      )

      lam <- Gen.const(Lambda(ps, body, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Function(pTps.map(_.typ), body.meta.typ.typ)))))

    } yield lam

  def genArg(tp: Type)(decls: Decls): Gen[Expr[NamePosType]] = {
    val candidateDecls = decls.collect {
      case Let(nm, _, candidateMeta @ NamePosType(_, _, TypeScheme(_, `tp`))) =>
        Reference(List.empty, nm, candidateMeta)
    }

    val litGen = tp match {
      case Type.Int => intGen
      case Type.Long => longGen
      case Type.Float => fltGen
      case Type.Double => dblGen
      case Type.Boolean => boolGen
      case Type.Char => charGen
      case Type.String => strGen
      case Type.Unit => unitGen
      case _ => fail("Unknown argument type")
    }

    if (candidateDecls.isEmpty)
      litGen
    else
      Gen.oneOf(candidateDecls)
  }

  def applyGen(lambdaDecls: Decls)(decls: Decls): Gen[Expr[NamePosType]] =
    for {
      lam <- Gen.oneOf(lambdaDecls)

      Let(nm, Lambda(_, _, _), lambdaMeta) = lam

      TypeScheme(_, TypeApply(TypeConstructor("->", _), tpArgs)) = lambdaMeta.typ

      args <- tpArgs.init.traverse(tp => genArg(tp)(decls))

    } yield Apply(Reference(List.empty, nm, lambdaMeta), args, NamePosType(NoName, Pos.Empty, TypeScheme(tpArgs.last)))

  def ifGen(decls: Decls): Gen[Expr[NamePosType]] = {
    val condDecls: List[Gen[Expr[NamePosType]]] = boolGen :: decls.collect {
      case Let(nm, _, condMeta @ NamePosType(_, _, TypeScheme(_, Type.Boolean))) =>
        Reference(List.empty, nm, condMeta)
    }.map(Gen.const)

    val condGen = Gen.oneOf(condDecls).flatMap(identity)

    for {
      condExpr <- condGen
      thenExpr <- exprGen(decls)
      elseExpr <- exprGen(decls).suchThat(_.meta.typ == thenExpr.meta.typ)
    } yield If(condExpr, thenExpr, elseExpr, NamePosType(NoName, Pos.Empty, thenExpr.meta.typ))
  }

  def ascriptionGen(decls: Decls): Gen[Expr[NamePosType]] = {
    Gen.delay(exprGen(decls)).map { expr =>
      Ascription(expr, expr.meta.typ, expr.meta)
    }
  }

  def exprGen(decls: Decls): Gen[Expr[NamePosType]] = {
    val lambdaDecls = decls.collect {
      case lambdaDecl @ Let(_, Lambda(_, _, _), _) => lambdaDecl
    }

    val applyGens =
      if (lambdaDecls.nonEmpty) List(applyGen(lambdaDecls)(decls)) else List.empty

    val noRefExprGens =
      literalGens ++ applyGens :+ lambdaGen(decls) :+ ifGen(decls) :+ ascriptionGen(decls)

    val exprGens =
      if (decls.isEmpty)
        noRefExprGens
      else
        noRefExprGens :+ referenceGen(decls)

    Gen.oneOf(exprGens)
      .flatMap(identity)
  }

  def letGen(modName: ModuleName, decls: Decls) =
    for {
      // Make sure we don't generate duplicate names
      name <- nameGen.suchThat(nm => !decls.map(_.name).contains(nm))
      expr <- exprGen(decls)
    } yield Let(name, expr, NamePosType(MemberName(modName.pkg, modName.cls, name), Pos.Empty, expr.meta.typ))

  def constructorGen(modName: ModuleName, dataType: TypeScheme) =
    for {
      name <- nameGen

      numArgs <- Gen.choose(1, 4)

      pNms <- Gen.listOfN(numArgs, nameGen).suchThat(vs => vs.distinct.length == vs.length)

      pTps <- Gen.listOfN(numArgs, Gen.oneOf(
                            TypeScheme(Type.Int),
                            TypeScheme(Type.Long),
                            TypeScheme(Type.Float),
                            TypeScheme(Type.Double),
                            TypeScheme(Type.Boolean),
                            TypeScheme(Type.Char),
                            TypeScheme(Type.String),
                            TypeScheme(Type.Unit),
                            dataType))

      params = pNms.lazyZip(pTps).map {
        case (nm, tp) =>
          Param(nm, Some(tp), NamePosType(LocalName(nm), Pos.Empty, tp))
      }

      typeScheme = TypeScheme(List.empty, Type.Function(pTps.map(_.typ), dataType.typ))

    } yield DataConstructor(name, params, typeScheme, NamePosType(MemberName(modName.pkg, modName.cls, name), Pos.Empty, typeScheme))

  def dataGen(modName: ModuleName) =
    for {
      name <- nameGen

      numConstrs <- Gen.choose(1, 4)

      typeScheme = TypeScheme(List.empty, TypeConstructor(name, List.empty))

      constrs <- Gen.listOfN(numConstrs, constructorGen(modName, typeScheme))

    } yield Data(name, List.empty, constrs, NamePosType(NoName, Pos.Empty, typeScheme))

  def declGen(modName: ModuleName) =
    StateT.modifyF[Gen, (Decls, Int)] {
      case (decls, remaining) =>
        val declGens = List(letGen(modName, decls), dataGen(modName))
        Gen.oneOf(declGens).flatMap { declGen =>
          declGen.map { decl => (decls :+ decl, remaining - 1) }
        }
    }

  def declsGen(modName: ModuleName): Gen[Decls] =
    for {
      size <- Gen.choose(0, 9)
      decls <- declGen(modName).get.flatMap {
        case (existingDecls, remaining) =>
          if (remaining < 0)
            StateT.pure[Gen, (Decls, Int), Decls](existingDecls)
          else
            declGen(modName).get.map(_._1)
      }.runA((List.empty, size))
    } yield decls

  implicit val arbitraryModule: Arbitrary[Module[NamePosType]] = Arbitrary {
    for {
      pkgLen <- Gen.choose(0, 5)
      pkg <- Gen.resize(pkgLen, Gen.listOfN(pkgLen, nameGen))
      name <- nameGen
      modName = ModuleName(pkg, name)
      decls <- declsGen(modName)
    } yield Module(pkg, name, List.empty, decls, NamePosType(modName, Pos.Empty, TypeScheme(Type.Module)))
  }
}
