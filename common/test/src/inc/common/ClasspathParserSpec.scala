package inc.common

import better.files._
import java.io.File
import org.scalatest._

class ClasspathParserSpec extends FlatSpec with Matchers {
  val entries = List(
    ".\\AppData\\Local\\Coursier\\cache\\v1\\https\\repo1.maven.org\\maven2\\org\\typelevel\\paiges-core_2.12\\0.2.1\\paiges-core_2.12-0.2.1.jar",
    ".\\AppData\\Local\\Coursier\\cache\\v1\\https\\repo1.maven.org\\maven2\\com\\lihaoyi\\fastparse-utils_2.12\\1.0.0\\fastparse-utils_2.12-1.0.0.jar",
    ".\\AppData\\Local\\Coursier\\cache\\v1\\https\\repo1.maven.org\\maven2\\com\\lihaoyi\\sourcecode_2.12\\0.1.4\\sourcecode_2.12-0.1.4.jar",
    ".\\AppData\\Local\\Coursier\\cache\\v1\\https\\repo1.maven.org\\maven2\\com\\github\\pathikrit\\better-files_2.12\\3.6.0\\better-files_2.12-3.6.0.jar",
    ".\\AppData\\Local\\Coursier\\cache\\v1\\https\\repo1.maven.org\\maven2\\com\\thesamet\\scalapb\\lenses_2.12\\0.8.0-RC1\\lenses_2.12-0.8.0-RC1.jar",
    ".\\AppData\\Local\\Coursier\\cache\\v1\\https\\repo1.maven.org\\maven2\\com\\google\\protobuf\\protobuf-java\\3.5.1\\protobuf-java-3.5.1.jar",
    ".\\AppData\\Local\\Coursier\\cache\\v1\\https\\repo1.maven.org\\maven2\\org\\scala-lang\\scala-library\\2.12.4\\scala-library-2.12.4.jar",
    ".\\AppData\\Local\\Coursier\\cache\\v1\\https\\repo1.maven.org\\maven2\\com\\thesamet\\scalapb\\scalapb-runtime_2.12\\0.8.0-RC1\\scalapb-runtime_2.12-0.8.0-RC1.jar",
    ".\\AppData\\Local\\Coursier\\cache\\v1\\https\\repo1.maven.org\\maven2\\com\\lihaoyi\\fastparse_2.12\\1.0.0\\fastparse_2.12-1.0.0.jar",
  )

  val classpathString = entries.mkString(File.pathSeparator)

  "ClasspathParser" should "parse a list of jar files as Archives" in {
    ClasspathParser.parse(classpathString) shouldBe entries.map(_.toFile).map(Archive)
  }

}
