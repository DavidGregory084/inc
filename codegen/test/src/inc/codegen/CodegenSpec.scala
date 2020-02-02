package inc.codegen

import java.io.ByteArrayOutputStream

import better.files._
import cats.implicits._
import inc.common._
import java.net.URLClassLoader
import org.scalatest._
import org.scalatestplus.scalacheck._

class CodegenSpec extends FlatSpec with Matchers with ScalaCheckDrivenPropertyChecks with Generators {

  def mkModule(name: String, decls: List[TopLevelDeclaration[Meta.Typed]]) = Module(
    pkg = List("Test", "Codegen"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = Meta.Typed(ModuleName(List("Test", "Codegen"), name), TypeScheme(Type.Module), Pos.Empty))

  def mkLet(name: String, binding: Expr[Meta.Typed]) =
    Let(name, binding, Meta.Typed(LocalName(name), binding.meta.typ, Pos.Empty))

  def mkInt(i: Int) = LiteralInt(i, Meta.Typed(NoName, TypeScheme(Type.Int), Pos.Empty))
  def mkRef(r: String, typ: TypeScheme) = Reference(List.empty, r, Meta.Typed(NoName, typ, Pos.Empty))
  def mkUnit() = LiteralUnit(Meta.Typed(NoName, TypeScheme(Type.Unit), Pos.Empty))

  "Codegen" should "generate code for a simple module" in {
    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int2", mkRef("int", TypeScheme(Type.Int))),
      mkLet("int3", mkRef("int2", TypeScheme(Type.Int)))
    ))

    val result = Codegen.generate(mod, Environment.empty).fold(
      errs => fail(s"""Code generation failed with errors ${errs.mkString(", ")}"""),
      identity
    )

    val baos = new ByteArrayOutputStream()

    Codegen.print(result.head.bytes, baos)

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

    val result = Codegen.generate(mod, Environment.empty)

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

    val result = Codegen.generate(mod, Environment.empty).fold(
      errs => fail(s"""Code generation failed with errors ${errs.mkString(", ")}"""),
      identity
    )

    Codegen.readInterface(result.head.bytes) shouldBe Right(mod.map(_.forgetPos))
  }

  def withTmpDir[A](test: File => A) = {
    val dir = File.newTemporaryDirectory()
    try test(dir)
    finally dir.delete()
  }

  it should "round trip arbitrary module files" in forAll(minSuccessful(1000)) { mod: Module[Meta.Typed] =>
    withTmpDir { dir =>
      try {
        val classFiles = Codegen.generate(mod, Environment.empty).fold(
          errs => fail(s"""Code generation failed with errors ${errs.mkString(", ")}"""),
          identity
        )

        Codegen.readInterface(classFiles.head.bytes) shouldBe Right(mod.map(_.forgetPos))

        val outDir = mod.pkg.foldLeft(dir) {
          case (path, next) => path / next
        }

        val outFiles = classFiles.map { classFile =>
          val outFile = outDir / s"${classFile.name}.class"

          if (outFile.exists)
            outFile.delete()

          outFile
            .createIfNotExists(createParents = true)
            .writeByteArray(classFile.bytes)

          outFile
        }

        val classLoader = Thread.currentThread.getContextClassLoader.asInstanceOf[URLClassLoader]

        val childLoader = URLClassLoader.newInstance(Array(dir.url), classLoader)

        val pkg = if (mod.pkg.isEmpty) "" else mod.pkg.mkString(".") + "."

        try {
          Class.forName(s"${pkg + outFiles.head.nameWithoutExtension}", true, childLoader)
        } catch {
          case e: Throwable =>
            classFiles.foreach(classFile => Codegen.print(classFile.bytes))
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
