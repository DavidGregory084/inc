package inc.codegen

import java.io.ByteArrayOutputStream

import inc.common._
import org.scalatest._
import org.scalatest.prop._

class CodegenSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
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
}
