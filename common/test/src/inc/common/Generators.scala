package inc.common

import cats.data.StateT
import cats.implicits._
import org.scalatest._
import org.scalacheck._
import org.scalacheck.cats.implicits._

trait Generators { self: Matchers =>
  type Decl = TopLevelDeclaration[NameWithType]
  type Decls = List[TopLevelDeclaration[NameWithType]]

  val nameGen: Gen[String] =
    for {
      len <- Gen.choose(0, 5)
      first <- Gen.alphaChar
      rest <- Gen.resize(len, Gen.alphaNumStr)
      nm = (first +: rest).mkString
      if !List("module", "import", "let", "if", "then", "else").contains(nm)
    } yield nm

  // Don't generate negative numbers: the language doesn't have operators yet so no parsing of prefix negation
  val intGen: Gen[Expr[NameWithType]] =
    Arbitrary.arbitrary[Int].filter(_ >= 0).map(LiteralInt(_, Pos.Empty, NameWithType(NoName, TypeScheme(Type.Int))))
  val longGen: Gen[Expr[NameWithType]] =
    Arbitrary.arbitrary[Long].filter(_ >= 0L).map(LiteralLong(_, Pos.Empty, NameWithType(NoName, TypeScheme(Type.Long))))
  val fltGen: Gen[Expr[NameWithType]] =
    Arbitrary.arbitrary[Float].filter(_ >= 0F).map(LiteralFloat(_, Pos.Empty, NameWithType(NoName, TypeScheme(Type.Float))))
  val dblGen: Gen[Expr[NameWithType]] =
    Arbitrary.arbitrary[Double].filter(_ >= 0D).map(LiteralDouble(_, Pos.Empty, NameWithType(NoName, TypeScheme(Type.Double))))
  val boolGen: Gen[Expr[NameWithType]] =
    Arbitrary.arbitrary[Boolean].map(LiteralBoolean(_, Pos.Empty, NameWithType(NoName, TypeScheme(Type.Boolean))))
  val charGen: Gen[Expr[NameWithType]] =
    Arbitrary.arbitrary[Char].map(LiteralChar(_, Pos.Empty, NameWithType(NoName, TypeScheme(Type.Char))))
  val strGen: Gen[Expr[NameWithType]] =
    Gen.asciiPrintableStr.filterNot { str =>
      str.contains('\n') ||
      str.contains('\r') ||
      str.contains('\\') ||
      str.contains('"')
    }.map(LiteralString(_, Pos.Empty, NameWithType(NoName, TypeScheme(Type.String))))
  val unitGen: Gen[Expr[NameWithType]] =
    Gen.const(LiteralUnit(Pos.Empty, NameWithType(NoName, TypeScheme(Type.Unit))))

  val literalGens: List[Gen[Expr[NameWithType]]] = List(intGen, longGen, fltGen, dblGen, boolGen, charGen, strGen, unitGen)

  def referenceGen(decls: Decls): Gen[Expr[NameWithType]] =
    Gen.oneOf(decls).map { existing =>
      Reference(existing.name, Pos.Empty, NameWithType(existing.meta.name, existing.meta.typ))
    }

  def lambdaGen(decls: Decls): Gen[Expr[NameWithType]] =
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
          Param(nm, Pos.Empty, NameWithType(LocalName(nm), tp))
      }
      body <- exprGen(
        // Unpleasant trick to allow later generators to refer to v
        decls ++ ps.map { p =>
          Let(p.name, Reference(p.name, Pos.Empty, p.meta), Pos.Empty, p.meta)
        }
      )
      lam <- Gen.const(Lambda(ps, body, Pos.Empty, NameWithType(NoName, TypeScheme(Type.Function(pTps.map(_.typ), body.meta.typ.typ)))))
    } yield lam

  def genArg(tp: Type)(decls: Decls): Gen[Expr[NameWithType]] = {
    val candidateDecls = decls.collect {
      case Let(nm, _, _, candidateMeta @ NameWithType(_, TypeScheme(_, `tp`))) =>
        Reference(nm, Pos.Empty, candidateMeta)
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

  def applyGen(lambdaDecls: Decls)(decls: Decls): Gen[Expr[NameWithType]] =
    for {
      lam <- Gen.oneOf(lambdaDecls)

      Let(nm, Lambda(_, _, _, _), _, lambdaMeta) = lam

      TypeScheme(_, TypeConstructor("->", tpArgs)) = lambdaMeta.typ

      args <- tpArgs.init.traverse(tp => genArg(tp)(decls))

    } yield Apply(Reference(nm, Pos.Empty, lambdaMeta), args, Pos.Empty, NameWithType(NoName, TypeScheme(tpArgs.last)))

  def ifGen(decls: Decls): Gen[Expr[NameWithType]] = {
    val condDecls: List[Gen[Expr[NameWithType]]] = boolGen :: decls.collect {
      case Let(nm, _, _, condMeta @ NameWithType(_, TypeScheme(_, Type.Boolean))) =>
        Reference(nm, Pos.Empty, condMeta)
    }.map(Gen.const)

    val condGen = Gen.oneOf(condDecls).flatMap(identity)

    for {
      condExpr <- condGen
      thenExpr <- exprGen(decls)
      elseExpr <- exprGen(decls).suchThat(_.meta.typ == thenExpr.meta.typ)
    } yield If(condExpr, thenExpr, elseExpr, Pos.Empty, NameWithType(NoName, thenExpr.meta.typ))
  }

  def exprGen(decls: Decls): Gen[Expr[NameWithType]] = {
    val lambdaDecls = decls.collect {
      case lambdaDecl @ Let(_, Lambda(_, _, _, _), _, _) => lambdaDecl
    }

    val applyGens =
      if (lambdaDecls.nonEmpty) List(applyGen(lambdaDecls)(decls)) else List.empty

    val exprGens =
      if (decls.isEmpty)
        literalGens ++ applyGens :+ lambdaGen(decls) :+ ifGen(decls)
      else
        literalGens ++ applyGens :+ lambdaGen(decls) :+ referenceGen(decls) :+ ifGen(decls)

    Gen.oneOf(exprGens)
      .flatMap(identity)
  }

  def letGen(modName: ModuleName, decls: Decls) =
    for {
      // Make sure we don't generate duplicate names
      name <- nameGen.suchThat(nm => !decls.map(_.name).contains(nm))
      expr <- exprGen(decls)
    } yield Let(name, expr, Pos.Empty, NameWithType(MemberName(modName.pkg, modName.cls, name), expr.meta.typ))

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

  implicit val arbitraryModule: Arbitrary[Module[NameWithType]] = Arbitrary {
    for {
      pkgLen <- Gen.choose(0, 5)
      pkg <- Gen.resize(pkgLen, Gen.listOfN(pkgLen, nameGen))
      name <- nameGen
      modName = ModuleName(pkg, name)
      decls <- declsGen(modName)
      // impLen <- Gen.choose(0, 5)
      // imports <- Gen.resize(impLen, Gen.listOfN(impLen, importGen))
    } yield Module(pkg, name, List.empty, decls, Pos.Empty, NameWithType(modName, TypeScheme(Type.Module)))
  }
}
