package inc.codegen

import java.io.ByteArrayOutputStream

import inc.common._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._

class CodegenSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  val nameGen: Gen[String] =
    for {
      first <- Gen.alphaChar
      rest <- Gen.alphaNumStr
    } yield (first +: rest).mkString

  val importGen: Gen[Import] =
    for {
      pkg <- Gen.containerOf[Vector, String](nameGen)
      symbols <- Arbitrary.arbitrary[Boolean]
      name <- nameGen
      imp <- {
        if (!symbols)
          Gen.const(ImportModule(pkg, name))
        else
          Gen.containerOf[Vector, String](nameGen).flatMap { syms =>
            ImportSymbols(pkg, name, syms)
          }
      }
    } yield imp

  implicit val arbitraryModule: Arbitrary[Module[NameWithType]] = Arbitrary {
    for {
      pkg <- Gen.containerOf[Vector, String](nameGen)
      name <- nameGen
      imports <- Gen.containerOf[Vector, Import](importGen)
    } yield Module(pkg, name, imports, Vector.empty, NameWithType(FullName(pkg, name), Type.Module))
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
    // TODO: decide how to handle round tripping of the empty package
    whenever(mod.pkg.nonEmpty) {
      val result = Codegen.generate(mod)
      result shouldBe 'right
      Codegen.readInterface(result.right.get) shouldBe Some(mod)
    }
  }
}
