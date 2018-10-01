package inc.codegen

import java.io.ByteArrayOutputStream

import better.files._
import cats.data.StateT
import inc.common._
import java.net.URLClassLoader
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._
import org.scalacheck.cats.implicits._

class CodegenSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  type Decl = TopLevelDeclaration[NameWithType]
  type Decls = List[TopLevelDeclaration[NameWithType]]

  val nameGen: Gen[String] =
    for {
      len <- Gen.choose(0, 5)
      first <- Gen.alphaChar
      rest <- Gen.resize(len, Gen.alphaNumStr)
    } yield (first +: rest).mkString

  val intGen: Gen[Expr[NameWithType]] = Arbitrary.arbitrary[Int].map(LiteralInt(_, NameWithType(NoName, TypeScheme(Type.Int))))
  val longGen: Gen[Expr[NameWithType]] = Arbitrary.arbitrary[Long].map(LiteralLong(_, NameWithType(NoName, TypeScheme(Type.Long))))
  val fltGen: Gen[Expr[NameWithType]] = Arbitrary.arbitrary[Float].map(LiteralFloat(_, NameWithType(NoName, TypeScheme(Type.Float))))
  val dblGen: Gen[Expr[NameWithType]] = Arbitrary.arbitrary[Double].map(LiteralDouble(_, NameWithType(NoName, TypeScheme(Type.Double))))
  val boolGen: Gen[Expr[NameWithType]] = Arbitrary.arbitrary[Boolean].map(LiteralBoolean(_, NameWithType(NoName, TypeScheme(Type.Boolean))))
  val charGen: Gen[Expr[NameWithType]] = Arbitrary.arbitrary[Char].map(LiteralChar(_, NameWithType(NoName, TypeScheme(Type.Char))))
  val strGen: Gen[Expr[NameWithType]] = Arbitrary.arbitrary[String].map(LiteralString(_, NameWithType(NoName, TypeScheme(Type.String))))
  val unitGen: Gen[Expr[NameWithType]] = Gen.const(LiteralUnit(NameWithType(NoName, TypeScheme(Type.Unit))))

  val literalGens: List[Gen[Expr[NameWithType]]] = List(intGen, longGen, fltGen, dblGen, boolGen, charGen, strGen, unitGen)

  def referenceGen(decls: Decls): Gen[Expr[NameWithType]] =
    Gen.oneOf(decls).map { existing =>
      Reference(existing.name, NameWithType(existing.meta.name, existing.meta.typ))
    }

  def lambdaGen(decls: Decls): Gen[Expr[NameWithType]] =
    for {
      v <- nameGen
      vTp <- Gen.oneOf(
        TypeScheme(Type.Int),
        TypeScheme(Type.Long),
        TypeScheme(Type.Float),
        TypeScheme(Type.Double),
        TypeScheme(Type.Boolean),
        TypeScheme(Type.Char),
        TypeScheme(Type.String),
        TypeScheme(Type.Unit))
      body <- exprGen(
        // Unpleasant trick to allow later generators to refer to v
        decls :+ Let(v, Reference(v, NameWithType(LocalName(v), vTp)), NameWithType(LocalName(v), vTp)),
        // Don't generate lambda because because we can't do first class functions yet
        generateFunctions = false
      )
      lam <- Gen.const(Lambda(v, body, NameWithType(NoName, TypeScheme(Type.Function(vTp.typ, body.meta.typ.typ)))))
    } yield lam

  def applyGen(lambdaDecls: Decls)(decls: Decls): Gen[Expr[NameWithType]] =
    for {
      lam <- Gen.oneOf(lambdaDecls)

      Let(nm, Lambda(_, _, _), lambdaMeta) = lam

      TypeScheme(_, TypeConstructor("->", List(from, to))) = lam.meta.typ

      candidateDecls = decls.collect {
        case Let(nm, _, candidateMeta @ NameWithType(_, TypeScheme(_, `from`))) =>
          Reference(nm, candidateMeta)
      }

      litGen = from match {
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

      argGen <- if (candidateDecls.nonEmpty) Gen.oneOf(candidateDecls.map(Gen.const)) else Gen.const(litGen)

      arg <- argGen

    } yield Apply(Reference(nm, lambdaMeta), List(arg), NameWithType(NoName, TypeScheme(to)))

  def exprGen(decls: Decls, generateFunctions: Boolean = true): Gen[Expr[NameWithType]] = {
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
        literalGens ++ lambdaGens ++ applyGens
      else
        // Don't generate reference to lambda because because we can't do first class functions yet
        literalGens ++ lambdaGens ++ applyGens :+ referenceGen(nonLambdaDecls)

    Gen.oneOf(exprGens)
      .flatMap(identity)
  }

  def letGen(modName: ModuleName, decls: Decls) =
    for {
      // Make sure we don't generate duplicate names
      name <- nameGen.suchThat(nm => !decls.map(_.name).contains(nm))
      expr <- exprGen(decls)
    } yield Let(name, expr, NameWithType(MemberName(modName.pkg, modName.cls, name), expr.meta.typ))

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

  implicit val arbitraryModule: Arbitrary[Module[NameWithType]] = Arbitrary {
    for {
      pkgLen <- Gen.choose(0, 5)
      pkg <- Gen.resize(pkgLen, Gen.listOfN(pkgLen, nameGen))
      name <- nameGen
      modName = ModuleName(pkg, name)
      decls <- declsGen(modName)
      impLen <- Gen.choose(0, 5)
      imports <- Gen.resize(impLen, Gen.listOfN(impLen, importGen))
    } yield Module(pkg, name, imports, decls, NameWithType(modName, TypeScheme(Type.Module)))
  }

  def mkModule(name: String, decls: List[TopLevelDeclaration[NameWithType]]) = Module(
    pkg = List("Test", "Codegen"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = NameWithType(ModuleName(List("Test", "Codegen"), name), TypeScheme(Type.Module)))

  "Codegen" should "generate code for a simple module" in {
    val mod = mkModule("Ref", List(
      Let("int", LiteralInt(42, NameWithType(NoName, TypeScheme(Type.Int))), NameWithType(LocalName("int"), TypeScheme(Type.Int))),
      Let("int2", Reference("int", NameWithType(NoName, TypeScheme(Type.Int))), NameWithType(LocalName("int2"), TypeScheme(Type.Int))),
      Let("int3", Reference("int2", NameWithType(NoName, TypeScheme(Type.Int))), NameWithType(LocalName("int3"), TypeScheme(Type.Int)))
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
      |""".stripMargin
    )
  }

  it should "generate code for a module with a Unit field reference" in {
    val mod = mkModule("Ref", List(
      Let("unit", LiteralUnit(NameWithType(NoName, TypeScheme(Type.Unit))), NameWithType(LocalName("unit"), TypeScheme(Type.Unit))),
      Let("unit2", Reference("unit", NameWithType(NoName, TypeScheme(Type.Unit))), NameWithType(LocalName("unit2"), TypeScheme(Type.Unit)))
    ))
    val result = Codegen.generate(mod)
    result shouldBe 'right
  }

  it should "parse a module definition from a generated class file" in {
    val mod = mkModule("Ref", List(
      Let("int", LiteralInt(42, NameWithType(NoName, TypeScheme(Type.Int))), NameWithType(LocalName("int"), TypeScheme(Type.Int))),
      Let("int2", Reference("int", NameWithType(NoName, TypeScheme(Type.Int))), NameWithType(LocalName("int2"), TypeScheme(Type.Int))),
      Let("int3", Reference("int2", NameWithType(NoName, TypeScheme(Type.Int))), NameWithType(LocalName("int3"), TypeScheme(Type.Int)))
    ))

    val result = Codegen.generate(mod)

    result shouldBe 'right

    Codegen.readInterface(result.right.get) shouldBe Some(mod)
  }

  def withTmpDir[A](test: File => A) = {
    val dir = File.newTemporaryDirectory()
    try test(dir)
    finally dir.delete()
  }

  it should "round trip arbitrary module files" in forAll { mod: Module[NameWithType] =>
    println(Printer.print(mod).render(80))

    withTmpDir { dir =>
      val result = Codegen.generate(mod)
      result shouldBe 'right

      Codegen.readInterface(result.right.get) shouldBe Some(mod)

      Codegen.print(result.right.get)

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

      Class.forName(s"${pkg + out.nameWithoutExtension}", true, childLoader)
    }
  }
}
