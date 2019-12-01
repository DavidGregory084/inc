package inc.codegen

import java.io.ByteArrayOutputStream

import better.files._
import cats.implicits._
import inc.common._
import java.net.URLClassLoader
import org.scalatest._
import org.scalatestplus.scalacheck._

class CodegenSpec extends FlatSpec with Matchers with ScalaCheckDrivenPropertyChecks with Generators {

  def mkModule(name: String, decls: List[TopLevelDeclaration[NamePosType]]) = Module(
    pkg = List("Test", "Codegen"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = NamePosType(ModuleName(List("Test", "Codegen"), name), Pos.Empty, TypeScheme(Type.Module)))

  def mkLet(name: String, binding: Expr[NamePosType]) =
    Let(name, binding, NamePosType(LocalName(name), Pos.Empty, binding.meta.typ))

  def mkInt(i: Int) = LiteralInt(i, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Int)))
  def mkRef(r: String, typ: TypeScheme) = Reference(List.empty, r, NamePosType(NoName, Pos.Empty, typ))
  def mkUnit() = LiteralUnit(NamePosType(NoName, Pos.Empty, TypeScheme(Type.Unit)))

  "Codegen" should "generate code for a simple module" in {
    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int2", mkRef("int", TypeScheme(Type.Int))),
      mkLet("int3", mkRef("int2", TypeScheme(Type.Int)))
    ))

    val result = Codegen.generate(mod).fold(
      errs => fail(s"""Code generation failed with errors ${errs.mkString(", ")}"""),
      identity
    )

    val baos = new ByteArrayOutputStream()

    Codegen.print(result, baos)

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

    result.fold(
      errs => fail(s"""Code generation failed with errors ${errs.mkString(", ")}"""),
      _    => succeed
    )
  }

  it should "parse a module definition from a generated class file" in {
    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int2", mkRef("int", TypeScheme(Type.Int))),
      mkLet("int3", mkRef("int2", TypeScheme(Type.Int)))
    ))

    val result = Codegen.generate(mod).fold(
      errs => fail(s"""Code generation failed with errors ${errs.mkString(", ")}"""),
      identity
    )

    Codegen.readInterface(result) shouldBe Right(mod.map(_.forgetPos))
  }

  def withTmpDir[A](test: File => A) = {
    val dir = File.newTemporaryDirectory()
    try test(dir)
    finally dir.delete()
  }

  it should "round trip arbitrary module files" in forAll(minSuccessful(1000)) { mod: Module[NamePosType] =>
    withTmpDir { dir =>
      try {
        val result = Codegen.generate(mod).fold(
          errs => fail(s"""Code generation failed with errors ${errs.mkString(", ")}"""),
          identity
        )

        Codegen.readInterface(result) shouldBe Right(mod.map(_.forgetPos))

        val outDir = mod.pkg.foldLeft(dir) {
          case (path, next) => path / next
        }

        val out = outDir / s"${mod.name}.class"

        out
          .createIfNotExists(createParents = true)
          .writeByteArray(result)

        val classLoader = Thread.currentThread.getContextClassLoader.asInstanceOf[URLClassLoader]

        val childLoader = URLClassLoader.newInstance(Array(dir.url), classLoader)

        val pkg = if (mod.pkg.isEmpty) "" else mod.pkg.mkString(".") + "."

        try {
          Class.forName(s"${pkg + out.nameWithoutExtension}", true, childLoader)
        } catch {
          case e: Throwable =>
            Codegen.print(result)
            throw e
        }
      } catch {
        case e: Throwable =>
          println(Printer.print(mod).render(80))
          e.printStackTrace()
          throw e
      }
    }
  }
}
