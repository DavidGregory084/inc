package inc.codegen

import java.io.ByteArrayOutputStream

import better.files._
import cats.implicits._
import inc.common._
import java.net.URLClassLoader
import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class CodegenSpec extends ScalaCheckSuite with Generators {

  def mkModule(name: String, decls: List[TopLevelDeclaration[Meta.Typed]]) = Module(
    pkg = List("Test", "Codegen"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = Meta.Typed(ModuleName(List("Test", "Codegen"), name), TypeScheme(Type.Module), Pos.Empty))

  def mkLet(name: String, binding: Expr[Meta.Typed]) =
    Let(name, binding, Meta.Typed(LocalName(name), binding.meta.typ, Pos.Empty))

  def mkInt(i: Int) = LiteralInt(i, Meta.Typed(NoName, TypeScheme(Type.Int), Pos.Empty))
  def mkRef(r: String, env: Map[String, Name], typ: TypeScheme) = Reference(List.empty, r, Meta.Typed(env(r), typ, Pos.Empty))
  def mkUnit() = LiteralUnit(Meta.Typed(NoName, TypeScheme(Type.Unit), Pos.Empty))

  test("Codegen should generate code for a simple module") {
    val env = Map(
      "int" -> MemberName(List("Test", "Codegen"), "Ref", "int"),
      "int2" -> MemberName(List("Test", "Codegen"), "Ref", "int2"),
      "int3" -> MemberName(List("Test", "Codegen"), "Ref", "int3")
    )

    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int2", mkRef("int", env, TypeScheme(Type.Int))),
      mkLet("int3", mkRef("int2", env, TypeScheme(Type.Int)))
    ))

    val result = Codegen.generate(mod, Environment.empty).fold(
      errs => fail(s"""Code generation failed with errors ${errs.mkString(", ")}"""),
      identity
    )

    val baos = new ByteArrayOutputStream()

    Codegen.print(result.head.bytes, baos)

    assertEquals(
      baos.toString,
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

  test("Codegen should generate code for a module with a Unit field reference") {
    val env = Map(
      "unit" -> MemberName(List("Test", "Codegen"), "Ref", "unit"),
      "unit2" -> MemberName(List("Test", "Codegen"), "Ref", "unit2")
    )

    val mod = mkModule("Ref", List(
      mkLet("unit", mkUnit()),
      mkLet("unit2", mkRef("unit", env, TypeScheme(Type.Unit)))
    ))

    val result = Codegen.generate(mod, Environment.empty)

    result.fold(
      errs => fail(s"""Code generation failed with errors ${errs.mkString(", ")}"""),
      _    => ()
    )
  }

  test("Codegen should parse a module definition from a generated class file") {
    val env = Map(
      "int" -> MemberName(List("Test", "Codegen"), "Ref", "int"),
      "int2" -> MemberName(List("Test", "Codegen"), "Ref", "int2"),
      "int3" -> MemberName(List("Test", "Codegen"), "Ref", "int3")
    )

    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int2", mkRef("int", env, TypeScheme(Type.Int))),
      mkLet("int3", mkRef("int2", env, TypeScheme(Type.Int)))
    ))

    val result = Codegen.generate(mod, Environment.empty).fold(
      errs => fail(s"""Code generation failed with errors ${errs.mkString(", ")}"""),
      identity
    )

    assertEquals(
      Codegen.readInterface(result.head.bytes),
      Right(mod.map(_.forgetPos))
    )
  }

  def withTmpDir[A](test: File => A) = {
    val dir = File.newTemporaryDirectory()
    try test(dir)
    finally dir.delete()
  }

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1000)

  property("Codegen should round trip arbitrary module files") {
    forAll { mod: Module[Meta.Typed] =>
      withTmpDir { dir =>
        try {
          val classFiles = Codegen.generate(mod, Environment.empty).fold(
            errs => fail(s"""Code generation failed with errors ${errs.mkString(", ")}"""),
            identity
          )

          assertEquals(
            Codegen.readInterface(classFiles.head.bytes),
            Right(mod.map(_.forgetPos))
          )

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
            assert(Class.forName(s"${pkg + outFiles.head.nameWithoutExtension}", true, childLoader) != null)
          } catch {
            case e: Throwable =>
              classFiles.foreach(classFile => Codegen.print(classFile.bytes))
              throw e
          }
        } catch {
          case e: Throwable =>
            println(Printer.print(mod).render(80))
            throw e
        }
      }
    }
  }
}
