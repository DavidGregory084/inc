package inc.main

import better.files._
import java.net.URLClassLoader
import org.scalatest._
import org.scalatest.prop._
import inc.rts.{ Unit => IncUnit }

class MainSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  def loadClassFrom(classFileDir: File, className: String) = {
    val classLoader = Thread.currentThread.getContextClassLoader.asInstanceOf[URLClassLoader]
    val childLoader = URLClassLoader.newInstance(Array(classFileDir.url), classLoader)
    Class.forName(s"${className}", true, childLoader)
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

  def withTmpDir[A](test: File => A) = {
    val dir = File.newTemporaryDirectory()
    try test(dir)
    finally dir.delete()
  }

  def shouldCompileField[A](fieldName: String, stringValue: String, expectedValue: A) = withTmpDir { dir =>
    val pkg = "Test.Main."
    val prog = s"module ${pkg}${fieldName.capitalize} { let ${fieldName} = ${stringValue} }"
    val classFile = Main.compileProgram(dir, prog).fold(err => fail(err.head), identity)
    val clazz = loadClassFrom(dir, pkg + classFile.nameWithoutExtension)
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

  it should "compile a false boolean field" in shouldCompileField("boolean", "true", true)

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
    val pkg = "Test.Main."
    val fieldName = "reference"
    val prog = s"module ${pkg}Reference { let integer = 42; let ${fieldName} = integer }"
    val result = Main.compileProgram(dir, prog)
    result shouldBe 'right
    val classFile = result.right.get
    val clazz = loadClassFrom(dir, pkg + classFile.nameWithoutExtension)
    getStatic(clazz, fieldName) shouldBe 42
  }
}
