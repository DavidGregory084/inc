package inc.main

import better.files._
import java.lang.Throwable
import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import inc.common._
import inc.rts.{Unit => IncUnit}
import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class MainSpec extends ScalaCheckSuite with Generators {
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
    val classFile = Main.compileModule(dir, s"${fieldName.capitalize}.inc", config, mod).value.fold(err => fail(err.head.message), identity)
    val clazz = loadClassFrom(dir, pkg + classFile.get.head.toFile.toScala.nameWithoutExtension)
    assertEquals(getStatic(clazz, fieldName), expectedValue)
  }

  test("Main should compile an integer field") { shouldCompileField("integer", "42", 42) }

  test("Main should compile a long field") { shouldCompileField("long", "42l", 42L) }

  test("Main should recognise both long suffixes") { shouldCompileField("long", "42L", 42L) }

  test("Main should compile a float field") { shouldCompileField("float", "3.142f", 3.142f) }

  test("Main should recognise both float suffixes") { shouldCompileField("float", "3.142F", 3.142f) }

  test("Main should compile a double field") { shouldCompileField("double", "3.142d", 3.142d) }

  test("Main should recognise both double suffixes") { shouldCompileField("double", "3.142D", 3.142d) }

  test("Main should default to double when there is no suffix") { shouldCompileField("double", "3.142", 3.142d) }

  test("Main should compile a true boolean field") { shouldCompileField("boolean", "true", true) }

  test("Main should compile a false boolean field") { shouldCompileField("boolean", "false", false) }

  property("Main should compile a char field") {
    forAll { c: Char =>
      (c != '\'' && c != '\\' && c != '\n' && c != '\r') ==> {
        shouldCompileField("char", s"'$c'", c)
      }
    }
  }

  property("Main should compile a string field") {
    forAll { s: String =>
      !(s.contains('\"') || s.contains('\\') || s.contains('\n') || s.contains('\r')) ==> {
        shouldCompileField("string", "\"" + s + "\"", s)
      }
    }
  }

  test("Main should compile a unit field"){ shouldCompileField("unit", "()", IncUnit.instance) }

  test("Main should compile a variable reference") {
    withTmpDir { dir =>
      val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
      val pkg = "Test/Main/"
      val fieldName = "reference"
      val mod = s"module ${pkg}Reference { let integer = 42; let ${fieldName} = integer }"
      val result = Main.compileModule(dir, s"${fieldName.capitalize}.inc", config, mod).value.fold(
        errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
        identity
      )
      val clazz = loadClassFrom(dir, pkg + result.get.head.toFile.toScala.nameWithoutExtension)
      assertEquals(
        getStatic(clazz, fieldName),
        42
      )
    }
  }

  test("Main should compile an if expression and return the then branch when the condition is true") {
    withTmpDir { dir =>
      val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
      val pkg = "Test/Main/"
      val fieldName = "z"
      val mod = s"module ${pkg}If { let a = true; let x = 42; let y = 41; let ${fieldName} = if a then x else y }"
      val result = Main.compileModule(dir, "If.inc", config, mod).value.fold(
        errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
        identity
      )
      val clazz = loadClassFrom(dir, pkg + result.get.head.toFile.toScala.nameWithoutExtension)
      assertEquals(
        getStatic(clazz, fieldName),
        42
      )
    }
  }

  test("Main should compile an if expression and return the else branch when the condition is false") {
    withTmpDir { dir =>
      val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
      val pkg = "Test/Main/"
      val fieldName = "z"
      val mod = s"module ${pkg}If { let a = false; let x = 42; let y = 41; let ${fieldName} = if a then x else y }"
      val result = Main.compileModule(dir, "If.inc", config, mod).value.fold(
        errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
        identity
      )
      val clazz = loadClassFrom(dir, pkg + result.get.head.toFile.toScala.nameWithoutExtension)
      assertEquals(getStatic(clazz, fieldName), 41)
    }
  }

  test("Main should compile a lambda expression") {
    withTmpDir { dir =>
      val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
      val mod = "module Test/Main/Lambda { let x = 42; let y = 41; let lam = bool -> if bool then x else y }"
      val result = Main.compileModule(dir, "Lambda.inc", config, mod).value.fold(
        errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
        identity
      )
      assert(loadClassFrom(dir, "Test.Main." + result.get.head.toFile.toScala.nameWithoutExtension) != null)
    }
  }

  test("Main should compile an identity function") {
    withTmpDir { dir =>
      val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
      val mod = "module Test/Main/Lambda { let id = a -> a }"
      val result = Main.compileModule(dir, "Lambda.inc", config, mod).value.fold(
        errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
        identity
      )
      assert(loadClassFrom(dir, "Test.Main." + result.get.head.toFile.toScala.nameWithoutExtension) != null)
    }
  }

  test("Main should compile an application of an identity function with a reference type") {
    withTmpDir { dir =>
      val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
      val mod = """module Test/Main/Lambda { let id = a -> a; let str = id("string") }"""
      val result = Main.compileModule(dir, "Lambda.inc", config, mod).value.fold(
        errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
        identity
      )
      assert(loadClassFrom(dir, "Test.Main." + result.get.head.toFile.toScala.nameWithoutExtension) != null)
    }
  }

  test("Main should compile an application of an identity function with a primitive type") {
    withTmpDir { dir =>
      val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
      val mod = """module Test/Main/Lambda { let id = a -> a; let int = id(1) }"""
      val result = Main.compileModule(dir, "Lambda.inc", config, mod).value.fold(
        errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
        identity
      )
      assert(loadClassFrom(dir, "Test.Main." + result.get.head.toFile.toScala.nameWithoutExtension) != null)
    }
  }

  test("Main should compile a function that accepts a function as argument") {
    withTmpDir { dir =>
      val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
      val mod = """module Test/Main/Const { let const = (a, b) -> a; let foo = a -> "a"; let bar = f -> foo(f(42, 36)); let baz = foo(const) }"""
      val result = Main.compileModule(dir, "Const.inc", config, mod).value.fold(
        errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
        identity
      )
      assert(loadClassFrom(dir, "Test.Main." + result.get.head.toFile.toScala.nameWithoutExtension) != null)
    }
  }

  test("Main should compile a module that imports from another module") {
    withTmpDir { dir =>
      val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)

      val mod1 =
        """
        |module Test/Id {
        |  let id = a -> a
        |}""".trim.stripMargin

      Main.compileModule(dir, "Id.inc", config, mod1).value.fold(
        errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
        identity
      )

      val mod2 =
        """
        |module Test/Apply {
        |  import Test/Id
        |  let int = Id.id(1)
        |}
        """.trim.stripMargin

      val result2 = Main.compileModule(dir, "Apply.inc", config, mod2).value.fold(
        errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
        identity
      )

      val clazz = loadClassFrom(dir, "Test." + result2.get.head.toFile.toScala.nameWithoutExtension)

      assertEquals(getStatic(clazz, "int"), 1)
    }
  }

  property("Main should compile arbitrary well-typed modules"){ 
    withTmpDir { dir =>
      val modGen = arbitraryModule.arbitrary.map(_.copy(pkg = List("Test", "Main")))
      forAll(modGen) { generatedMod =>
        val config = Configuration.test.copy(classpath = dir.toUri.toURL.toString)
        val mod = Printer.print(generatedMod).render(80)
        try {
          val result = Main.compileModule(dir, s"${generatedMod.name}.inc", config, mod).value.fold(
            errs => fail(s"""Compilation failed with errors ${errs.mkString(", ")}"""),
            identity
          )
          assert(loadClassFrom(dir, "Test.Main." + result.get.head.toFile.toScala.nameWithoutExtension) != null)
        } catch {
          case e: Throwable =>
            println(NL + mod)
            e.printStackTrace
            throw e
        }
      }
    }
  }
}
