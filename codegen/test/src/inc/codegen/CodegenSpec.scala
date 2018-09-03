package inc.codegen

import java.io.ByteArrayOutputStream

import cats.data.StateT
import inc.common._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._
import org.scalacheck.cats.implicits._

class CodegenSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  type Decl = TopLevelDeclaration[NameWithType]
  type Decls = Vector[TopLevelDeclaration[NameWithType]]

  val nameGen: Gen[String] =
    for {
      first <- Gen.alphaChar
      rest <- Gen.alphaNumStr
    } yield (first +: rest).mkString

  val literalGens = Vector(
    Arbitrary.arbitrary[Int].map(LiteralInt(_, NameWithType(NoName, Type.Int))),
    Arbitrary.arbitrary[Long].map(LiteralLong(_, NameWithType(NoName, Type.Long))),
    Arbitrary.arbitrary[Float].map(LiteralFloat(_, NameWithType(NoName, Type.Float))),
    Arbitrary.arbitrary[Double].map(LiteralDouble(_, NameWithType(NoName, Type.Double))),
    Arbitrary.arbitrary[Boolean].map(LiteralBoolean(_, NameWithType(NoName, Type.Boolean))),
    Arbitrary.arbitrary[Char].map(LiteralChar(_, NameWithType(NoName, Type.Char))),
    Arbitrary.arbitrary[String].map(LiteralString(_, NameWithType(NoName, Type.String))),
    Gen.const(LiteralUnit(NameWithType(NoName, Type.Unit)))
  )

  def referenceGen(decls: Decls) =
    Gen.oneOf(decls).map { existing =>
      Reference(existing.name, NameWithType(NoName, existing.meta.typ))
    }

  def exprGen(decls: Decls) = {
    val exprGens =
      if (decls.isEmpty)
        literalGens
      else
        literalGens :+ referenceGen(decls)

    Gen.oneOf(exprGens)
      .flatMap(identity)
  }

  def letGen(decls: Decls) =
    for {
      name <- nameGen
      expr <- exprGen(decls)
    } yield Let(name, expr, NameWithType(LocalName(name), expr.meta.typ))

  val declGen =
    StateT.modifyF[Gen, (Decls, Int)] {
      case (decls, remaining) =>
        letGen(decls).map { decl =>
          (decls :+ decl, remaining - 1)
        }
    }

  val declsGen: Gen[Decls] =
    for {
      size <- Gen.choose(0, 9)
      decls <- declGen.get.flatMap {
        case (existingDecls, remaining) =>
          if (remaining < 0)
            StateT.pure[Gen, (Decls, Int), Decls](existingDecls)
          else
            declGen.get.map(_._1)
      }.runA((Vector.empty, size))
    } yield decls

  val importGen =
    for {
      pkgLen <- Gen.choose(0, 5)
      pkg <- Gen.resize(pkgLen, Gen.containerOfN[Vector, String](pkgLen, nameGen))
      symbols <- Arbitrary.arbitrary[Boolean]
      name <- nameGen
      imp <- {
        if (!symbols)
          Gen.const(ImportModule(pkg, name))
        else
          for {
            symLen <- Gen.choose(2, 5)
            syms <- Gen.resize(symLen, Gen.containerOfN[Vector, String](symLen, nameGen))
          } yield ImportSymbols(pkg, name, syms)
      }
    } yield imp

  implicit val arbitraryModule: Arbitrary[Module[NameWithType]] = Arbitrary {
    for {
      pkgLen <- Gen.choose(0, 5)
      pkg <- Gen.resize(pkgLen, Gen.containerOfN[Vector, String](pkgLen, nameGen))
      name <- nameGen
      decls <- declsGen
      impLen <- Gen.choose(0, 5)
      imports <- Gen.resize(impLen, Gen.containerOfN[Vector, Import](impLen, importGen))
    } yield Module(pkg, name, imports, decls, NameWithType(FullName(pkg, name), Type.Module))
  }

  def mkModule(name: String, decls: Seq[TopLevelDeclaration[NameWithType]]) = Module(
    pkg = Seq("Test", "Codegen"),
    name = name,
    imports = Seq.empty,
    declarations = decls,
    meta = NameWithType(FullName(Seq("Test", "Codegen"), name), Type.Module))

  "Codegen" should "generate code for a simple module" in {
    val mod = mkModule("Ref", Seq(
      Let("int", LiteralInt(42, NameWithType(NoName, Type.Int)), NameWithType(LocalName("int"), Type.Int)),
      Let("int2", Reference("int", NameWithType(NoName, Type.Int)), NameWithType(LocalName("int2"), Type.Int)),
      Let("int3", Reference("int2", NameWithType(NoName, Type.Int)), NameWithType(LocalName("int3"), Type.Int))
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
    val mod = mkModule("Ref", Seq(
      Let("unit", LiteralUnit(NameWithType(NoName, Type.Unit)), NameWithType(LocalName("unit"), Type.Unit)),
      Let("unit2", Reference("unit", NameWithType(NoName, Type.Unit)), NameWithType(LocalName("unit2"), Type.Unit))
    ))
    val result = Codegen.generate(mod)
    result shouldBe 'right
  }

  it should "parse a module definition from a generated class file" in {
    val mod = mkModule("Ref", Seq(
      Let("int", LiteralInt(42, NameWithType(NoName, Type.Int)), NameWithType(LocalName("int"), Type.Int)),
      Let("int2", Reference("int", NameWithType(NoName, Type.Int)), NameWithType(LocalName("int2"), Type.Int)),
      Let("int3", Reference("int2", NameWithType(NoName, Type.Int)), NameWithType(LocalName("int3"), Type.Int))
    ))

    val result = Codegen.generate(mod)

    result shouldBe 'right

    Codegen.readInterface(result.right.get) shouldBe Some(mod)
  }

  it should "round trip arbitrary module files" in forAll { mod: Module[NameWithType] =>
    val result = Codegen.generate(mod)
    result shouldBe 'right
    Codegen.readInterface(result.right.get) shouldBe Some(mod)
  }
}
