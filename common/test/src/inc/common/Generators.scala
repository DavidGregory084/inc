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
    Arbitrary.arbitrary[Int].filter(_ >= 0).map(LiteralInt(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Int))))
  val longGen: Gen[Expr[NamePosType]] =
    Arbitrary.arbitrary[Long].filter(_ >= 0L).map(LiteralLong(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Long))))
  val fltGen: Gen[Expr[NamePosType]] =
    Arbitrary.arbitrary[Float].filter(_ >= 0F).map(LiteralFloat(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Float))))
  val dblGen: Gen[Expr[NamePosType]] =
    Arbitrary.arbitrary[Double].filter(_ >= 0D).map(LiteralDouble(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Double))))
  val boolGen: Gen[Expr[NamePosType]] =
    Arbitrary.arbitrary[Boolean].map(LiteralBoolean(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Boolean))))
  val charGen: Gen[Expr[NamePosType]] =
    Arbitrary.arbitrary[Char].map(LiteralChar(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Char))))
  val strGen: Gen[Expr[NamePosType]] =
    Gen.asciiPrintableStr.filterNot { str =>
      str.contains('\n') ||
      str.contains('\r') ||
      str.contains('\\') ||
      str.contains('"')
    }.map(LiteralString(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.String))))
  val unitGen: Gen[Expr[NamePosType]] =
    Gen.const(LiteralUnit(NamePosType(NoName, Pos.Empty, TypeScheme(Type.Unit))))

  val literalGens: List[Gen[Expr[NamePosType]]] = List(intGen, longGen, fltGen, dblGen, boolGen, charGen, strGen, unitGen)

  def referenceGen(decls: Decls): Gen[Expr[NamePosType]] =
    Gen.oneOf(decls).map { existing =>
      Reference(existing.name, NamePosType(existing.meta.name, Pos.Empty, existing.meta.typ))
    }

  def lambdaGen(decls: Decls): Gen[Expr[NamePosType]] =
    for {
      numArgs <- Gen.choose(1, 4)
      // Don't generate duplicate variable names
      pNms <- Gen.listOfN(numArgs, nameGen).suchThat(vs => vs.distinct.length == vs.length)
      pTps <- Gen.listOfN(numArgs, Gen.oneOf(
        TypeScheme(Type.Int),
        TypeScheme(Type.Long),
        TypeScheme(Type.Float),
        TypeScheme(Type.Double),
        TypeScheme(Type.Boolean),
        TypeScheme(Type.Char),
        TypeScheme(Type.String),
        TypeScheme(Type.Unit)))
      ps = pNms.zip(pTps).map {
        case (nm, tp) =>
          Param(nm, NamePosType(LocalName(nm), Pos.Empty, tp))
      }
      body <- exprGen(
        // Trick to allow later generators to refer to params;
        // we need to filter out any existing declarations that
        // clash with our params, since params can shadow top level declarations
        decls.filterNot(d => !ps.exists(_.name == d.name)) ++ ps.map { p =>
          Let(p.name, Reference(p.name, p.meta), p.meta)
        }
      )
      lam <- Gen.const(Lambda(ps, body, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Function(pTps.map(_.typ), body.meta.typ.typ)))))
    } yield lam

  def genArg(tp: Type)(decls: Decls): Gen[Expr[NamePosType]] = {
    val candidateDecls = decls.collect {
      case Let(nm, _, candidateMeta @ NamePosType(_, _, TypeScheme(_, `tp`))) =>
        Reference(nm, candidateMeta)
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

      TypeScheme(_, TypeConstructor("->", tpArgs)) = lambdaMeta.typ

      args <- tpArgs.init.traverse(tp => genArg(tp)(decls))

    } yield Apply(Reference(nm, lambdaMeta), args, NamePosType(NoName, Pos.Empty, TypeScheme(tpArgs.last)))

  def ifGen(decls: Decls): Gen[Expr[NamePosType]] = {
    val condDecls: List[Gen[Expr[NamePosType]]] = boolGen :: decls.collect {
      case Let(nm, _, condMeta @ NamePosType(_, _, TypeScheme(_, Type.Boolean))) =>
        Reference(nm, condMeta)
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

  def declGen(modName: ModuleName) =
    StateT.modifyF[Gen, (Decls, Int)] {
      case (decls, remaining) =>
        letGen(modName, decls).map { decl =>
          (decls :+ decl, remaining - 1)
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

  // val importGen =
  //   for {
  //     pkgLen <- Gen.choose(0, 5)
  //     pkg <- Gen.resize(pkgLen, Gen.listOfN(pkgLen, nameGen))
  //     symbols <- Arbitrary.arbitrary[Boolean]
  //     name <- nameGen
  //     imp <- {
  //       if (!symbols)
  //         Gen.const(ImportModule(pkg, name))
  //       else
  //         for {
  //           symLen <- Gen.choose(2, 5)
  //           syms <- Gen.resize(symLen, Gen.listOfN(symLen, nameGen))
  //         } yield ImportSymbols(pkg, name, syms)
  //     }
  //   } yield imp

  implicit val arbitraryModule: Arbitrary[Module[NamePosType]] = Arbitrary {
    for {
      pkgLen <- Gen.choose(0, 5)
      pkg <- Gen.resize(pkgLen, Gen.listOfN(pkgLen, nameGen))
      name <- nameGen
      modName = ModuleName(pkg, name)
      decls <- declsGen(modName)
      // impLen <- Gen.choose(0, 5)
      // imports <- Gen.resize(impLen, Gen.listOfN(impLen, importGen))
    } yield Module(pkg, name, List.empty, decls, NamePosType(modName, Pos.Empty, TypeScheme(Type.Module)))
  }
}
