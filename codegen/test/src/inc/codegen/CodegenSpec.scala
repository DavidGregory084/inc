package inc.codegen

import java.io.ByteArrayOutputStream

import better.files._
import cats.data.StateT
import cats.implicits._
import inc.common._
import java.net.URLClassLoader
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._
import org.scalacheck.cats.implicits._

class CodegenSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  type Decl = TopLevelDeclaration[NamePosType]
  type Decls = List[TopLevelDeclaration[NamePosType]]

  val nameGen: Gen[String] =
    for {
      len <- Gen.choose(0, 5)
      first <- Gen.alphaChar
      rest <- Gen.resize(len, Gen.alphaNumStr)
    } yield (first +: rest).mkString

  val intGen: Gen[Expr[NamePosType]] = Arbitrary.arbitrary[Int].map(LiteralInt(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Int))))
  val longGen: Gen[Expr[NamePosType]] = Arbitrary.arbitrary[Long].map(LiteralLong(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Long))))
  val fltGen: Gen[Expr[NamePosType]] = Arbitrary.arbitrary[Float].map(LiteralFloat(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Float))))
  val dblGen: Gen[Expr[NamePosType]] = Arbitrary.arbitrary[Double].map(LiteralDouble(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Double))))
  val boolGen: Gen[Expr[NamePosType]] = Arbitrary.arbitrary[Boolean].map(LiteralBoolean(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Boolean))))
  val charGen: Gen[Expr[NamePosType]] = Arbitrary.arbitrary[Char].map(LiteralChar(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Char))))
  val strGen: Gen[Expr[NamePosType]] = Arbitrary.arbitrary[String].map(LiteralString(_, NamePosType(NoName, Pos.Empty, TypeScheme(Type.String))))
  val unitGen: Gen[Expr[NamePosType]] = Gen.const(LiteralUnit(NamePosType(NoName, Pos.Empty, TypeScheme(Type.Unit))))

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
        // Unpleasant trick to allow later generators to refer to v
        decls ++ ps.map { p =>
          Let(p.name, Reference(p.name, p.meta), p.meta)
        },
        // Don't generate lambda because because we can't do first class functions yet
        generateFunctions = false
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
      thenExpr <- exprGen(decls, generateFunctions = false)
      elseExpr <- exprGen(decls, generateFunctions = false).suchThat(_.meta.typ == thenExpr.meta.typ)
    } yield If(condExpr, thenExpr, elseExpr, NamePosType(NoName, Pos.Empty, thenExpr.meta.typ))
  }

  def exprGen(decls: Decls, generateFunctions: Boolean = true): Gen[Expr[NamePosType]] = {
    val lambdaGens =
      if (generateFunctions) List(lambdaGen(decls)) else List.empty

    val lambdaDecls = decls.collect {
      case lambdaDecl @ Let(_, Lambda(_, _, _), _) => lambdaDecl
    }

    val nonLambdaDecls = decls.filterNot(lambdaDecls.contains)

    val applyGens =
      if (lambdaDecls.nonEmpty) List(applyGen(lambdaDecls)(nonLambdaDecls)) else List.empty

    val exprGens =
      if (nonLambdaDecls.isEmpty)
        literalGens ++ lambdaGens ++ applyGens :+ ifGen(decls)
      else
        // Don't generate reference to lambda because because we can't do first class functions yet
        literalGens ++ lambdaGens ++ applyGens :+ referenceGen(nonLambdaDecls) :+ ifGen(decls)

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

  val importGen =
    for {
      pkgLen <- Gen.choose(0, 5)
      pkg <- Gen.resize(pkgLen, Gen.listOfN(pkgLen, nameGen))
      symbols <- Arbitrary.arbitrary[Boolean]
      name <- nameGen
      imp <- {
        if (!symbols)
          Gen.const(ImportModule(pkg, name))
        else
          for {
            symLen <- Gen.choose(2, 5)
            syms <- Gen.resize(symLen, Gen.listOfN(symLen, nameGen))
          } yield ImportSymbols(pkg, name, syms)
      }
    } yield imp

  implicit val arbitraryModule: Arbitrary[Module[NamePosType]] = Arbitrary {
    for {
      pkgLen <- Gen.choose(0, 5)
      pkg <- Gen.resize(pkgLen, Gen.listOfN(pkgLen, nameGen))
      name <- nameGen
      modName = ModuleName(pkg, name)
      decls <- declsGen(modName)
      impLen <- Gen.choose(0, 5)
      imports <- Gen.resize(impLen, Gen.listOfN(impLen, importGen))
    } yield Module(pkg, name, imports, decls, NamePosType(modName, Pos.Empty, TypeScheme(Type.Module)))
  }

  def mkModule(name: String, decls: List[TopLevelDeclaration[NamePosType]]) = Module(
    pkg = List("Test", "Codegen"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = NamePosType(ModuleName(List("Test", "Codegen"), name), Pos.Empty, TypeScheme(Type.Module)))

  def mkLet(name: String, binding: Expr[NamePosType]) =
    Let(name, binding, NamePosType(LocalName(name), Pos.Empty, binding.meta.typ))

  def mkInt(i: Int) = LiteralInt(i, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Int)))
  def mkRef(r: String, typ: TypeScheme) = Reference(r, NamePosType(NoName, Pos.Empty, typ))
  def mkUnit() = LiteralUnit(NamePosType(NoName, Pos.Empty, TypeScheme(Type.Unit)))

  "Codegen" should "generate code for a simple module" in {
    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int2", mkRef("int", TypeScheme(Type.Int))),
      mkLet("int3", mkRef("int2", TypeScheme(Type.Int)))
    ))
    val result = Codegen.generate(mod)

    result shouldBe 'right

    val baos = new ByteArrayOutputStream()

    Codegen.print(result.right.get, baos)

    baos.toString shouldBe (
      """// class version 52.0 (52)
      |// access flags 0x21
      |public class Test/Codegen/Ref {
      |
      |
      |  ATTRIBUTE IncInterface : unknown
      |
      |  // access flags 0x19
      |  public final static I int = 42
      |
      |  // access flags 0x19
      |  public final static I int2
      |
      |  // access flags 0x19
      |  public final static I int3
      |
      |  // access flags 0x8
      |  static <clinit>()V
      |    GETSTATIC Test/Codegen/Ref.int : I
      |    PUTSTATIC Test/Codegen/Ref.int2 : I
      |    GETSTATIC Test/Codegen/Ref.int2 : I
      |    PUTSTATIC Test/Codegen/Ref.int3 : I
      |    RETURN
      |    MAXSTACK = 1
      |    MAXLOCALS = 0
      |}
      |""".stripMargin.replaceAll("\\r\\n", "\n")
    )
  }

  it should "generate code for a module with a Unit field reference" in {
    val mod = mkModule("Ref", List(
      mkLet("unit", mkUnit()),
      mkLet("unit2", mkRef("unit", TypeScheme(Type.Unit)))
    ))
    val result = Codegen.generate(mod)
    result shouldBe 'right
  }

  it should "parse a module definition from a generated class file" in {
    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int2", mkRef("int", TypeScheme(Type.Int))),
      mkLet("int3", mkRef("int2", TypeScheme(Type.Int)))
    ))

    val result = Codegen.generate(mod)

    result shouldBe 'right

    Codegen.readInterface(result.right.get) shouldBe Some(mod.map(_.forgetPos))
  }

  def withTmpDir[A](test: File => A) = {
    val dir = File.newTemporaryDirectory()
    try test(dir)
    finally dir.delete()
  }

  it should "round trip arbitrary module files" in forAll(minSuccessful(1000)) { mod: Module[NamePosType] =>
    withTmpDir { dir =>
      val result = Codegen.generate(mod)
      result shouldBe 'right

      Codegen.readInterface(result.right.get) shouldBe Some(mod.map(_.forgetPos))

      val outDir = mod.pkg.foldLeft(dir) {
        case (path, next) => path / next
      }

      val out = outDir / s"${mod.name}.class"

      out
        .createIfNotExists(createParents = true)
        .writeByteArray(result.right.get)

      val classLoader = Thread.currentThread.getContextClassLoader.asInstanceOf[URLClassLoader]

      val childLoader = URLClassLoader.newInstance(Array(dir.url), classLoader)

      val pkg = if (mod.pkg.isEmpty) "" else mod.pkg.mkString(".") + "."

      try {
        Class.forName(s"${pkg + out.nameWithoutExtension}", true, childLoader)
      } catch {
        case e: Throwable =>
          Codegen.print(result.right.get)
          throw e
      }
    }
  }
}
