package inc.main

import ammonite.ops._
import java.net.URLClassLoader
import org.scalatest._
import org.scalatest.prop._

class MainSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  def loadClassFrom(classFileDir: Path, className: String) = {
    val classLoader = Thread.currentThread.getContextClassLoader.asInstanceOf[URLClassLoader]
    val childLoader = URLClassLoader.newInstance(Array(classFileDir.toNIO.toUri.toURL), classLoader)
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

  def withTmpDir[A](test: Path => A) = {
    val dir = tmp.dir()
    try test(dir)
    finally rm! dir
  }

  def shouldCompileField[A](fieldName: String, stringValue: String, expectedValue: A) = withTmpDir { dir =>
    val pkg = "Test."
    val classFile = Main.compileProgram(dir, s"module ${pkg}${fieldName.capitalize} { let ${fieldName} = ${stringValue} }")
    val classFileName = classFile.name.dropRight(classFile.ext.length + 1)
    val clazz = loadClassFrom(dir, pkg + classFileName)
    getStatic(clazz, fieldName) shouldBe expectedValue
  }

  "Main" should "compile an integer field" in shouldCompileField("integer", "42", 42)

  it should "compile a long field" in shouldCompileField("long", "42l", 42L)

  it should "recognise both long suffixes" in shouldCompileField("long", "42L", 42L)

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
}
