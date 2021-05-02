import java.nio.file.attribute.PosixFilePermission
import mill._
import mill.define._
import mill.scalalib._
import mill.scalalib.publish._

import ammonite.ops._
import coursier.MavenRepository
import mill.api.{ Ctx, Result }
import mill.modules.Jvm
import mill.scalalib.api.CompilationResult
import mill.scalalib.Lib
import java.net.{ URI, URL, URLClassLoader }

import $ivy.`com.lihaoyi::mill-contrib-scalapblib:$MILL_VERSION`
import mill.contrib.scalapblib._

import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import mill.contrib.buildinfo.BuildInfo

import $ivy.`com.lihaoyi::mill-contrib-scoverage:$MILL_VERSION`
import mill.contrib.scoverage.ScoverageModule

import $ivy.`io.github.davidgregory084::mill-tpolecat:0.2.0`
import io.github.davidgregory084.TpolecatModule

import $ivy.`com.github.tototoshi::scala-csv:1.3.6`
import com.github.tototoshi.csv._

trait PublishSettingsModule extends PublishModule {
  def publishVersion = "0.1.0-SNAPSHOT"

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "io.github.davidgregory084",
    url = "https://github.com/DavidGregory084/inc",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("DavidGregory084", "inc"),
    developers = Seq(Developer("DavidGregory084", "David Gregory", "https://github.com/DavidGregory084"))
  )

}

trait ScalaSettingsModule extends TpolecatModule with PublishSettingsModule {
  def scalaVersion = "2.13.3"
  def catsVersion = T { "2.2.0" }
  def munitVersion = T { "0.7.10" }
  def pprintVersion = T { "0.6.0" }

  def scalacPluginIvyDeps = Agg(
    ivy"com.olegpy::better-monadic-for:0.3.1",
    ivy"org.typelevel:kind-projector_${scalaVersion()}:0.11.0",
    ivy"org.scalameta:semanticdb-scalac_${scalaVersion()}:4.3.20"
  )

  def scalacOptions = T {
    super.scalacOptions().filterNot(Set("-Xfatal-warnings")) ++ Seq(
      "-Yno-imports",
      "-Yrangepos",
      // "-opt:l:method",
      // "-opt:l:inline",
      // Inlining Float.parseFloat and Double.parseDouble causes
      // IllegalAccessError to jdk.internal.math.FloatingDecimal on JDK11
      // "-opt-inline-from:**:!java.lang.Double:!java.lang.Float"
    )
  }

  trait Test extends Tests {
    def ivyDeps = Agg(
      ivy"org.scalameta::munit::${munitVersion()}",
      ivy"org.scalameta::munit-scalacheck::${munitVersion()}",
      ivy"io.chrisdavenport::cats-scalacheck:0.3.0",
      ivy"com.lihaoyi::pprint:${pprintVersion()}",
      ivy"com.github.pathikrit::better-files:3.9.1",
      ivy"org.scalacheck::scalacheck:1.14.3"
    )
    def testFrameworks = Seq("munit.Framework")
    def scalacOptions = T { super.scalacOptions().filterNot(Set("-Yno-imports")) }
  }
}

object decompiled extends JavaModule {
  def moduleDeps = Seq(rts)
}

object common extends ScalaSettingsModule with ScoverageModule {
  def scoverageVersion = "1.4.1"
  def ivyDeps = T {
    super.ivyDeps() ++ Agg(
      ivy"io.bullet::borer-core:1.7.1",
      ivy"io.bullet::borer-derivation:1.7.1",
      ivy"com.lihaoyi::pprint:${pprintVersion()}",
      ivy"com.lihaoyi::sourcecode:0.2.1",
      ivy"org.typelevel::cats-core:${catsVersion()}",
      ivy"org.typelevel::paiges-core:0.3.1",
      ivy"com.typesafe.scala-logging::scala-logging:3.9.2"
    )
  }
  object test extends super.Test with ScoverageTests
}

object rts extends JavaModule with PublishSettingsModule

object parser extends ScalaSettingsModule with ScoverageModule {
  def scoverageVersion = "1.4.1"
  def moduleDeps = Seq(common)
  def ivyDeps = Agg(ivy"com.lihaoyi::fastparse:2.3.0")
  object test extends super.Test with ScoverageTests {
    override def moduleDeps =
      super.moduleDeps :+ common.test
  }
}

object resolver extends ScalaSettingsModule with ScoverageModule {
  def scoverageVersion = "1.4.1"
  def moduleDeps = Seq(common)
  object test extends super.Test with ScoverageTests {
    override def moduleDeps =
      super.moduleDeps :+ common.test
  }
}

object typechecker extends ScalaSettingsModule with ScoverageModule {
  def scoverageVersion = "1.4.1"
  def moduleDeps = Seq(common)
  object test extends super.Test with ScoverageTests {
    override def moduleDeps =
      super.moduleDeps :+ common.test
  }
}

object codegen extends ScalaSettingsModule with ScoverageModule {
  def scoverageVersion = "1.4.1"
  def moduleDeps = Seq(common, rts)
  def asmVersion = T { "8.0.1" }
  def ivyDeps = Agg(
    ivy"org.ow2.asm:asm:${asmVersion()}",
    ivy"org.ow2.asm:asm-commons:${asmVersion()}",
    ivy"org.ow2.asm:asm-util:${asmVersion()}"
  )
  object test extends super.Test with ScoverageTests {
    override def moduleDeps =
      super.moduleDeps :+ common.test
  }
}

object main extends ScalaSettingsModule with ScoverageModule with BuildInfo {
  def mainClass = Some("inc.main.Main")

  def scoverageVersion = "1.4.1"

  def moduleDeps = Seq(common, rts, parser, resolver, typechecker, codegen)

  def jlineVersion = T { "3.16.0" }

  def ivyDeps = Agg(
    ivy"org.jline:jline-terminal:${jlineVersion()}",
    ivy"org.jline:jline-terminal-jansi:${jlineVersion()}",
    ivy"org.apache.logging.log4j:log4j-slf4j-impl:2.13.3",
    ivy"com.github.scopt::scopt:3.7.1",
    ivy"com.lihaoyi::pprint:${pprintVersion()}",
    // PPrint definitely requires scala-reflect
    ivy"org.scala-lang:scala-reflect:${scalaVersion()}"
  )

  def scalacOptions = T {
    super.scalacOptions().filterNot(Set("-Yno-imports"))
  }

  def buildInfoPackageName = Some("inc.main")
  def buildInfoObjectName = "Build"
  def buildInfoMembers = T { Map("version" -> publishVersion()) }

  def shrinkBinary = T {
    val r8Conf = millSourcePath / up / "r8.conf"
    val outFile = T.dest / "out-r8.jar"

    Lib.resolveDependencies(
      repositories ++ Seq(MavenRepository("https://storage.googleapis.com/r8-releases/raw")),
      Lib.depToDependencyJava(_),
      Seq(ivy"com.android.tools:r8:2.1.66"),
    ).flatMap { dependencies =>
      try Result.Success(Jvm.runSubprocess(
        mainClass = "com.android.tools.r8.R8",
        classPath = dependencies.map(_.path),
        mainArgs = Seq(
          "--debug",
          "--classfile",
          "--pg-conf", r8Conf.toString,
          "--output", outFile.toString,
          "--lib", System.getProperty("java.home"),
          assembly().path.toString
        ),
        workingDir = T.dest
      )) catch {
        case e: Exception =>
          Result.Failure("shrinking with r8 failed")
      }
    }.map { _ =>
      PathRef(outFile)
    }
  }

  def generateRunScript() = T.command {
    // val shellScript = prependShellScript()
    // val lineSep = if (!shellScript.endsWith("\n")) "\n\r\n" else ""
    // val script = millSourcePath / up / "inc"
    // if (exists! script) rm!(script)

    // if (shellScript.isEmpty) {
    //   cp(shrinkBinary().path, script)
    // } else {
    //   write(script, shellScript + lineSep)
    //   write.append(script, os.read.inputStream(shrinkBinary().path))

    //   if (!scala.util.Properties.isWin) {
    //     os.perms.set(
    //       script,
    //       os.perms(script) +
    //         PosixFilePermission.GROUP_EXECUTE +
    //         PosixFilePermission.OWNER_EXECUTE +
    //         PosixFilePermission.OTHERS_EXECUTE
    //     )
    //   }
    // }

    val script = millSourcePath / up / "inc"
    if (exists! script) rm!(script)
    cp(assembly().path, script)
  }

  object test extends super.Test with ScoverageTests {
    override def moduleDeps =
      super.moduleDeps :+ common.test
  }
}

trait IncWorker {
  def compile(files: Seq[PathRef], args: String*)(implicit ctx: Ctx): Result[Unit]
}

class IncWorkerImpl(classpath: Seq[PathRef]) extends IncWorker {
  def compile(files: Seq[PathRef], args: String*)(implicit ctx: Ctx): Result[Unit] = {
    val classpathUrls = classpath.map(_.path.toIO.toURI().toURL()).toArray[URL]
    val classLoader = new URLClassLoader(classpathUrls, null)
    val clazz = classLoader.loadClass("inc.main.Main")
    val mainMethod = clazz.getMethod("main", classOf[Array[String]])
    files.map { f =>
      Result.create(mainMethod.invoke(null, (args :+ f.path.toIO.getAbsolutePath()).toArray[String]))
    }.collectFirst {
      case r @ Result.Exception(_, _) => r
      case r @ Result.Aborted => Result.Aborted
      case r @ Result.Skipped => Result.Skipped
      case r @ Result.Failure(res, _) => Result.Failure(res, None)
    }.getOrElse(Result.Success())
  }
}

trait IncModule extends ScalaModule {
  override def moduleDeps =
    super.moduleDeps :+ main

  override def scalaVersion = main.scalaVersion()

  override def allSourceFiles = T.sources {
    for {
      sources <- allSources()
      if os.exists(sources.path)
      path <- if (os.isDir(sources.path)) os.walk(sources.path) else Seq(sources.path)
      if os.isFile(path) && !path.last.startsWith(".") && path.ext.toLowerCase == "inc"
    } yield PathRef(path)
  }

  def incWorker: Worker[IncWorker] = T.worker {
    new IncWorkerImpl(compileClasspath().toSeq)
  }

  override def compile: T[CompilationResult] = T.task {
    val ctx = T.ctx()
    val dest = ctx.dest
    val classes = dest / "classes"
    os.makeDir.all(classes)

    val classpathString = compileClasspath()
      .map(_.path.toIO.toURI().toURL())
      .mkString(java.io.File.separator)

    val compileResults = incWorker().compile(
      allSourceFiles(),
      "--classpath", classpathString,
      "--destination", classes.toString,
      "--exit-on-error", "false"
    )

    val analysisFile = dest / "inc.analysis.dummy"
    os.write(target = analysisFile, data = "", createFolders = true)

    compileResults.map { _ => CompilationResult(analysisFile, PathRef(classes)) }
  }

  trait Test extends Tests {
    def ivyDeps = Agg(ivy"com.novocode:junit-interface:0.11")
    def testFrameworks = Seq("com.novocode.junit.JUnitFramework")
  }
}

object bench extends IncModule {
  override def allSources = T.sources { millSourcePath }
  object test extends super.Test
}
