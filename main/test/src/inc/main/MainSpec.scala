package inc.main

import better.files._
import java.lang.Throwable
import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import org.scalatest._
import org.scalatestplus.scalacheck._
import inc.common._
import inc.rts.{Unit => IncUnit}

class MainSpec extends FlatSpec with Matchers with ScalaCheckDrivenPropertyChecks with Generators {
  def loadClassFrom(classFileDir: File, moduleName: String) = {
    val className = moduleName.replaceAll("/", ".")
    val classLoader = Thread.currentThread.getContextClassLoader
    val childLoader = URLClassLoader.newInstance(Array(classFileDir.url), classLoader)
    try Class.forName(s"${className}", true, childLoader) catch {
      case e: Throwable =>
        e.printStackTrace()
        fail("Unable to load compiled class", e)
    }
  }

  def getStatic(clazz: Class[_], name: String) = {
    clazz.getDeclaredField(name).get(null)
  }

  def invokeStatic(clazz: Class[_], name: String, args: (Class[_], AnyRef)*) = {
    if (args.isEmpty)
      clazz.getDeclaredMethod(name).invoke(null)
    else {
      val (argClasses, argValues) = args.unzip
      clazz.getDeclaredMethod(name, argClasses: _*).invoke(null, argValues: _*)
    }
  }

  def withTmpDir[A](test: Path => A) = {
    val dir = Files.createTempDirectory("")
    try test(dir)
    finally {
      dir.toFile.toScala.delete()
      ()
    }
  }

  def shouldCompileField[A](fieldName: String, stringValue: String, expectedValue: A) = withTmpDir { dir =>
    val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
    val pkg = "Test/Main/"
    val mod = s"module ${pkg}${fieldName.capitalize} { let ${fieldName} = ${stringValue} }"
    val ctx = Printer.SourceContext(80, s"${fieldName.capitalize}.inc", mod)
    val classFile = Main.compileModule(dir, ctx, config).fold(err => fail(err.head), identity)
    val clazz = loadClassFrom(dir, pkg + classFile.head.toFile.toScala.nameWithoutExtension)
    getStatic(clazz, fieldName) shouldBe expectedValue
  }

  "Main" should "compile an integer field" in shouldCompileField("integer", "42", 42)

  it should "compile a long field" in shouldCompileField("long", "42l", 42L)

  it should "recognise both long suffixes" in shouldCompileField("long", "42L", 42L)

  it should "compile a float field" in shouldCompileField("float", "3.142f", 3.142f)

  it should "recognise both float suffixes" in shouldCompileField("float", "3.142F", 3.142f)

  it should "compile a double field" in shouldCompileField("double", "3.142d", 3.142d)

  it should "recognise both double suffixes" in shouldCompileField("double", "3.142D", 3.142d)

  it should "default to double when there is no suffix" in shouldCompileField("double", "3.142", 3.142d)

  it should "compile a true boolean field" in shouldCompileField("boolean", "true", true)

  it should "compile a false boolean field" in shouldCompileField("boolean", "false", false)

  it should "compile a char field" in forAll { c: Char =>
    whenever(c != '\'' && c != '\\' && c != '\n' && c != '\r') {
      shouldCompileField("char", s"'$c'", c)
    }
  }

  it should "compile a string field" in forAll { s: String =>
    whenever(!(s.contains('\"') || s.contains('\\') || s.contains('\n') || s.contains('\r'))) {
      shouldCompileField("string", "\"" + s + "\"", s)
    }
  }

  it should "compile a unit field" in shouldCompileField("unit", "()", IncUnit.instance)

  it should "compile a variable reference" in withTmpDir { dir =>
    val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
    val pkg = "Test/Main/"
    val fieldName = "reference"
    val mod = s"module ${pkg}Reference { let integer = 42; let ${fieldName} = integer }"
    val ctx = Printer.SourceContext(80, s"${fieldName.capitalize}.inc", mod)
    val result = Main.compileModule(dir, ctx, config).fold(
      errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
      identity
    )
    val clazz = loadClassFrom(dir, pkg + result.head.toFile.toScala.nameWithoutExtension)
    getStatic(clazz, fieldName) shouldBe 42
  }

  it should "compile an if expression and return the then branch when the condition is true" in withTmpDir { dir =>
    val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
    val pkg = "Test/Main/"
    val fieldName = "z"
    val mod = s"module ${pkg}If { let a = true; let x = 42; let y = 41; let ${fieldName} = if a then x else y }"
    val ctx = Printer.SourceContext(80, "If.inc", mod)
    val result = Main.compileModule(dir, ctx, config).fold(
      errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
      identity
    )
    val clazz = loadClassFrom(dir, pkg + result.head.toFile.toScala.nameWithoutExtension)
    getStatic(clazz, fieldName) shouldBe 42
  }

  it should "compile an if expression and return the else branch when the condition is false" in withTmpDir { dir =>
    val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
    val pkg = "Test/Main/"
    val fieldName = "z"
    val mod = s"module ${pkg}If { let a = false; let x = 42; let y = 41; let ${fieldName} = if a then x else y }"
    val ctx = Printer.SourceContext(80, "If.inc", mod)
    val result = Main.compileModule(dir, ctx, config).fold(
      errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
      identity
    )
    val clazz = loadClassFrom(dir, pkg + result.head.toFile.toScala.nameWithoutExtension)
    getStatic(clazz, fieldName) shouldBe 41
  }

  it should "compile a lambda expression" in withTmpDir { dir =>
    val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
    val mod = "module Test/Main/Lambda { let x = 42; let y = 41; let lam = bool -> if bool then x else y }"
    val ctx = Printer.SourceContext(80, "Lambda.inc", mod)
    val result = Main.compileModule(dir, ctx, config).fold(
      errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
      identity
    )
    loadClassFrom(dir, "Test.Main." + result.head.toFile.toScala.nameWithoutExtension)
  }

  it should "compile an identity function" in withTmpDir { dir =>
    val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
    val mod = "module Test/Main/Lambda { let id = a -> a }"
    val ctx = Printer.SourceContext(80, "Lambda.inc", mod)
    val result = Main.compileModule(dir, ctx, config).fold(
      errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
      identity
    )
    loadClassFrom(dir, "Test.Main." + result.head.toFile.toScala.nameWithoutExtension)
  }

  it should "compile an application of an identity function with a reference type" in withTmpDir { dir =>
    val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
    val mod = """module Test/Main/Lambda { let id = a -> a; let str = id("string") }"""
    val ctx = Printer.SourceContext(80, "Lambda.inc", mod)
    val result = Main.compileModule(dir, ctx, config).fold(
      errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
      identity
    )
    loadClassFrom(dir, "Test.Main." + result.head.toFile.toScala.nameWithoutExtension)
  }

  it should "compile an application of an identity function with a primitive type" in withTmpDir { dir =>
    val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
    val mod = """module Test/Main/Lambda { let id = a -> a; let int = id(1) }"""
    val ctx = Printer.SourceContext(80, "Lambda.inc", mod)
    val result = Main.compileModule(dir, ctx, config).fold(
      errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
      identity
    )
    loadClassFrom(dir, "Test.Main." + result.head.toFile.toScala.nameWithoutExtension)
  }

  it should "compile a function that accepts a function as argument" in withTmpDir { dir =>
    val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
    val mod = """module Test/Main/Const { let const = (a, b) -> a; let foo = a -> "a"; let bar = f -> foo(f(42, 36)); let baz = foo(const) }"""
    val ctx = Printer.SourceContext(80, "Const.inc", mod)
    val result = Main.compileModule(dir, ctx, config).fold(
      errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
      identity
    )
    loadClassFrom(dir, "Test.Main." + result.head.toFile.toScala.nameWithoutExtension)
  }

  it should "compile a module that imports from another module" in withTmpDir { dir =>
    val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)

    val mod1 =
      """
      |module Test/Id {
      |  let id = a -> a
      |}""".trim.stripMargin

    val ctx1 = Printer.SourceContext(80, "Id.inc", mod1)

    Main.compileModule(dir, ctx1, config).fold(
      errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
      _    => succeed
    )

    val mod2 =
      """
      |module Test/Apply {
      |  import Test/Id
      |  let int = Id.id(1)
      |}
      """.trim.stripMargin

    val ctx2 = Printer.SourceContext(80, "Apply.inc", mod2)

    val result2 = Main.compileModule(dir, ctx2, config).fold(
      errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
      identity
    )

    val clazz = loadClassFrom(dir, "Test." + result2.head.toFile.toScala.nameWithoutExtension)

    getStatic(clazz, "int") shouldBe 1
  }

  it should "compile arbitrary well-typed modules" in withTmpDir { dir =>
    val modGen = arbitraryModule.arbitrary.map(_.copy(pkg = List("Test", "Main")))
    forAll(modGen, minSuccessful(1000)) { generatedMod =>
      val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
      val mod = Printer.print(generatedMod).render(80)
      val ctx = Printer.SourceContext(80, s"${generatedMod.name}.inc", mod)
      try {
        val result = Main.compileModule(dir, ctx, config).fold(
          errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
          identity
        )
        loadClassFrom(dir, "Test.Main." + result.head.toFile.toScala.nameWithoutExtension)
      } catch {
        case e: Throwable =>
          println(NL + mod)
          e.printStackTrace
          throw e
      }
    }
  }
}
