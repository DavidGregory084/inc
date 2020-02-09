package inc.common

import cats.data.StateT
import cats.implicits._
import org.scalatest._
import org.scalacheck._
import org.scalacheck.cats.implicits._

trait Generators { self: Matchers =>
  type Decl = TopLevelDeclaration[Meta.Typed]
  type Decls = List[TopLevelDeclaration[Meta.Typed]]

  val nameGen: Gen[String] =
    for {
      len <- Gen.choose(0, 5)
      first <- Gen.alphaChar
      rest <- Gen.resize(len, Gen.alphaNumStr)
      nm = (first +: rest).mkString
      if !List("module", "import", "let", "if", "then", "else").contains(nm)
    } yield nm

  // Don't generate negative numbers: the language doesn't have operators yet so no parsing of prefix negation
  val intGen: Gen[Expr[Meta.Typed]] =
    Arbitrary.arbitrary[Int]
      .filter(_ >= 0)
      .map(LiteralInt(_, Meta.Typed(NoName, TypeScheme(Type.Int), Pos.Empty)))

  val longGen: Gen[Expr[Meta.Typed]] =
    Arbitrary.arbitrary[Long]
      .filter(_ >= 0L)
      .map(LiteralLong(_, Meta.Typed(NoName, TypeScheme(Type.Long), Pos.Empty)))

  val fltGen: Gen[Expr[Meta.Typed]] =
    Arbitrary.arbitrary[Float]
      .filter(_ >= 0F)
      .map(LiteralFloat(_, Meta.Typed(NoName, TypeScheme(Type.Float), Pos.Empty)))

  val dblGen: Gen[Expr[Meta.Typed]] =
    Arbitrary.arbitrary[Double]
      .filter(_ >= 0D)
      .map(LiteralDouble(_, Meta.Typed(NoName, TypeScheme(Type.Double), Pos.Empty)))

  val boolGen: Gen[Expr[Meta.Typed]] =
    Arbitrary.arbitrary[Boolean]
      .map(LiteralBoolean(_, Meta.Typed(NoName, TypeScheme(Type.Boolean), Pos.Empty)))

  val charGen: Gen[Expr[Meta.Typed]] =
    Gen.asciiPrintableChar.filterNot { chr =>
      // Parsing char and string escapes is not implemented yet
      chr == '\n' ||
        chr == '\r' ||
        chr == '\\' ||
        chr == '\''
    }.map(LiteralChar(_, Meta.Typed(NoName, TypeScheme(Type.Char), Pos.Empty)))

  val strGen: Gen[Expr[Meta.Typed]] =
    Gen.asciiPrintableStr.filterNot { str =>
      // Parsing char and string escapes is not implemented yet
      str.contains('\n') ||
      str.contains('\r') ||
      str.contains('\\') ||
      str.contains('"')
    }.map(LiteralString(_, Meta.Typed(NoName, TypeScheme(Type.String), Pos.Empty)))

  val unitGen: Gen[Expr[Meta.Typed]] =
    Gen.const(LiteralUnit(Meta.Typed(NoName, TypeScheme(Type.Unit), Pos.Empty)))

  val literalGens: List[Gen[Expr[Meta.Typed]]] =
    List(intGen, longGen, fltGen, dblGen, boolGen, charGen, strGen, unitGen)

  def referenceGen(decls: Decls): Gen[Expr[Meta.Typed]] =
    Gen.oneOf(decls).flatMap {
      case Let(name, _, meta) =>
        Gen.const(Reference(List.empty, name, Meta.Typed(meta.name, meta.typ, Pos.Empty)))
      case Data(_, _, cases, _) =>
        Gen.oneOf(cases).map {
          case DataConstructor(caseName, _, _, caseMeta) =>
            Reference(List.empty, caseName, Meta.Typed(caseMeta.name, caseMeta.typ, Pos.Empty))
        }
    }

  def lambdaGen(decls: Decls): Gen[Expr[Meta.Typed]] =
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
          Param(nm, if (ascribed) Some(tp) else None, Meta.Typed(LocalName(nm), tp, Pos.Empty))
      }

      body <- exprGen(
        // Trick to allow later generators to refer to params;
        // we need to filter out any existing declarations that
        // clash with our params, since params can shadow top level declarations
        decls.filterNot(d => ps.exists(_.name == d.name)) ++ ps.map { p =>
          Let(p.name, Reference(List.empty, p.name, p.meta), p.meta)
        }
      )

      lam <- Gen.const(Lambda(ps, body, Meta.Typed(NoName, TypeScheme(Type.Function(pTps.map(_.typ), body.meta.typ.typ)), Pos.Empty)))

    } yield lam

  def genArg(tp: Type)(decls: Decls): Gen[Expr[Meta.Typed]] = {
    val candidateDecls = decls.collect {
      case Let(nm, _, candidateMeta @ Meta.Typed(_, TypeScheme(_, `tp`), _)) =>
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

  def applyGen(lambdaDecls: Decls)(decls: Decls): Gen[Expr[Meta.Typed]] =
    for {
      lam <- Gen.oneOf(lambdaDecls)

      Let(nm, Lambda(_, _, _), lambdaMeta) = lam

      TypeScheme(_, TypeApply(TypeConstructor("->", _, _), tpArgs, _, _)) = lambdaMeta.typ

      args <- tpArgs.init.traverse(tp => genArg(tp)(decls))

    } yield Apply(Reference(List.empty, nm, lambdaMeta), args, Meta.Typed(NoName, TypeScheme(tpArgs.last), Pos.Empty))

  def ifGen(decls: Decls): Gen[Expr[Meta.Typed]] = {
    val condDecls: List[Gen[Expr[Meta.Typed]]] = boolGen :: decls.collect {
      case Let(nm, _, condMeta @ Meta.Typed(_, TypeScheme(_, Type.Boolean), _)) =>
        Reference(List.empty, nm, condMeta)
    }.map(Gen.const)

    val condGen = Gen.oneOf(condDecls).flatMap(identity)

    for {
      condExpr <- condGen
      thenExpr <- exprGen(decls)
      elseExpr <- exprGen(decls).suchThat(_.meta.typ == thenExpr.meta.typ)
    } yield If(condExpr, thenExpr, elseExpr, Meta.Typed(NoName, thenExpr.meta.typ, Pos.Empty))
  }

  def ascriptionGen(decls: Decls): Gen[Expr[Meta.Typed]] = {
    Gen.delay(exprGen(decls)).map { expr =>
      Ascription(expr, expr.meta.typ, expr.meta)
    }
  }

  def exprGen(decls: Decls): Gen[Expr[Meta.Typed]] = {
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
      name <- nameGen.suchThat(!collides(decls, _))
      expr <- exprGen(decls)
    } yield Let(name, expr, Meta.Typed(MemberName(modName.pkg, modName.mod, name), expr.meta.typ, Pos.Empty))

  def constructorGen(dataName: DataName, dataType: TypeScheme, decls: Decls) =
    for {
      name <- nameGen.suchThat(nm => nm != dataName.name && !collides(decls, nm))

      numArgs <- Gen.choose(1, 4)

      pNms <- Gen.listOfN(numArgs, nameGen).suchThat(vs =>  vs.distinct.length == vs.length)

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
          Param(nm, Some(tp), Meta.Typed(LocalName(nm), tp, Pos.Empty))
      }

      typeScheme = TypeScheme(List.empty, Type.Function(pTps.map(_.typ), dataType.typ))

    } yield DataConstructor(name, params, typeScheme, Meta.Typed(ConstrName(dataName.pkg, dataName.mod, dataName.name, name), typeScheme, Pos.Empty))

  def collides(decls: Decls, name: String): Boolean = {
    val names = for {
      decl <- decls
      member <- decl.members
      name <- member.name match {
        case DataName(_, _, name) => List(name)
        case MemberName(_, _, name) => List(name)
        case ConstrName(_, _, _, name) => List(name)
        case _ => List.empty
      }
    } yield name

    names.contains(name)
  }

  def dataGen(modName: ModuleName, decls: Decls) =
    for {
      name <- nameGen.suchThat(!collides(decls, _))

      numConstrs <- Gen.choose(1, 4)

      typeScheme = TypeScheme(List.empty, TypeConstructor(name, Atomic))

      dataName = DataName(modName.pkg, modName.mod, name)

      constrs <- Gen.listOfN(numConstrs, constructorGen(dataName, typeScheme, decls)).suchThat { cs =>
        // Don't generate duplicate names
        val constrNames = cs.map(_.name)
        constrNames.distinct.length == cs.length
      }

    } yield Data(name, List.empty, constrs, Meta.Typed(dataName, typeScheme, Pos.Empty))

  def declGen(modName: ModuleName) =
    StateT.modifyF[Gen, (Decls, Int)] {
      case (decls, remaining) =>
        val declGens = List(letGen(modName, decls), dataGen(modName, decls))
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

  implicit val arbitraryModule: Arbitrary[Module[Meta.Typed]] = Arbitrary {
    for {
      pkgLen <- Gen.choose(0, 5)
      pkg <- Gen.resize(pkgLen, Gen.listOfN(pkgLen, nameGen))
      name <- nameGen
      modName = ModuleName(pkg, name)
      decls <- declsGen(modName)
    } yield Module(pkg, name, List.empty, decls, Meta.Typed(modName, TypeScheme(Type.Module), Pos.Empty))
  }
}
