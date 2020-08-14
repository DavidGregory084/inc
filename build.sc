import mill._
import mill.scalalib._
import mill.scalalib.publish._

import ammonite.ops._
import mill.api.{ Ctx, Result }
import mill.define.Worker
import mill.scalalib.api.CompilationResult
import java.net.{ URI, URL, URLClassLoader }

import $ivy.`com.lihaoyi::mill-contrib-scalapblib:0.8.0`
import mill.contrib.scalapblib._

import $ivy.`com.lihaoyi::mill-contrib-buildinfo:0.8.0`
import mill.contrib.BuildInfo

import $ivy.`com.lihaoyi::mill-contrib-scoverage:0.8.0`
import mill.contrib.scoverage.ScoverageModule

import $ivy.`io.github.davidgregory084::mill-tpolecat:0.1.4`
import io.github.davidgregory084.TpolecatModule

import $ivy.`org.postgresql:postgresql:42.2.6`
import org.postgresql.copy.CopyManager
import org.postgresql.core.BaseConnection
import java.sql.DriverManager

import $ivy.`com.github.tototoshi::scala-csv:1.3.6`
import com.github.tototoshi.csv._

import java.io.FileReader
import java.time._
import java.time.format._

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
  def catsVersion = T { "2.1.1" }
  def munitVersion = T { "0.7.10" }
  def pprintVersion = T { "0.6.0" }

  def scalacPluginIvyDeps = Agg(
    ivy"com.olegpy::better-monadic-for:0.3.1",
    ivy"org.scalameta:semanticdb-scalac_${scalaVersion()}:4.3.20"
  )

  def scalacOptions = T {
    super.scalacOptions() ++ Seq(
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

object decompiled extends JavaModule

object proto extends ScalaPBModule with ScalaSettingsModule {
  def scalaPBVersion = "0.10.7"
  def scalaPBFlatPackage = true
  def scalaPBGrpc = false
  def scalaPBLenses = false
  def scalacOptions = T { super.scalacOptions().filterNot(Set("-Yno-imports")) }
}

object common extends ScalaSettingsModule with ScoverageModule {
  def scoverageVersion = "1.4.1"
  def moduleDeps = Seq(proto)
  def ivyDeps = T {
    super.ivyDeps() ++ Agg(
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
  object test extends super.Test with ScoverageTests
}

object resolver extends ScalaSettingsModule with ScoverageModule {
  def scoverageVersion = "1.4.1"
  def moduleDeps = Seq(common)
  object test extends super.Test with ScoverageTests
}

object typechecker extends ScalaSettingsModule with ScoverageModule {
  def scoverageVersion = "1.4.1"
  def moduleDeps = Seq(common)
  object test extends super.Test with ScoverageTests
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

  def buildInfoPackageName = Some("inc.main")
  def buildInfoObjectName = "Build"
  def buildInfoMembers = T { Map("version" -> publishVersion()) }

  def generateRunScript() = T.command {
    val script = millSourcePath / up / "inc"
    if (exists! script) rm!(script)
    cp(assembly().path, script)
  }

  def benchmarkSources = T.sources {
    millSourcePath / up / "bench"
  }

  def runBenchmark() = T.command {
    val dest = T.ctx().dest
    val assemblyJar = assembly()

    def isBuildkiteCI = {
      T.ctx().env
        .get("BUILDKITE")
        .nonEmpty
    }

    def currentBranchName(): String = {
      if (isBuildkiteCI)
        T.ctx().env("BUILDKITE_BRANCH")
      else
        os.proc('git, "rev-parse", "--abbrev-ref", 'HEAD)
          .call(millSourcePath)
          .out.string.trim
    }

    def currentCommitRef(): String = {
      if (isBuildkiteCI)
        T.ctx().env("BUILDKITE_COMMIT")
      else
        os.proc('git, "rev-parse", 'HEAD)
          .call(millSourcePath)
          .out.string.trim
    }

    def currentCommitTime(): ZonedDateTime = {
      val result = os.proc(
        'git, 'show, "-s",
        "--format=%cd",
        "--date=iso",
        currentCommitRef()
      ).call(millSourcePath)

      val timeStr = result.out.string.trim
      val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss ZZZ")
      ZonedDateTime.parse(timeStr, formatter).withZoneSameInstant(ZoneOffset.UTC)
    }

    def addRunData(baseName: String, time: ZonedDateTime): Unit = {
      val path = dest / (baseName + ".json")
      val json = ujson.read(os.read(path))
      json("results")(0)("executionTime") = time.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      json("results")(0)("commitRef") = currentCommitRef()
      json("results")(0)("branchName") = currentBranchName()
      json("results")(0)("commitTime") = currentCommitTime().format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      os.write.over(path, ujson.write(json, indent = 2))
    }

    def runHyperfine(src: os.Path): Unit = {
      T.ctx().log.info(s"Running benchmark $src")

      val executionTime = ZonedDateTime.now(ZoneOffset.UTC)

      os.proc(
        'hyperfine,
        s"java -jar ${assemblyJar.path.toIO.getAbsolutePath} ${src.toIO.getAbsolutePath}",
        s"--export-json",
        s"${dest / src.baseName}.json",
        "--ignore-failure"
      ).call(millSourcePath)

      addRunData(src.baseName, executionTime)
    }

    def runBenchmark(inputDir: os.Path): Unit = {
      T.ctx().log.info(s"Running benchmarks in $inputDir")
      if (inputDir.toIO.exists) {
        os.walk(inputDir)
          .filter(_.last.matches(".*.inc"))
          .foreach(runHyperfine)
      }
    }

    benchmarkSources().foreach(src => runBenchmark(src.path))

    mill.api.Result.Success(PathRef(dest))
  }

  def benchmarkCsv = T {
    val dest = T.ctx().dest
    val results = runBenchmark()()

    os.walk(results.path)
      .filter(_.last.matches(".*.json"))
      .map { src =>
        val csvFormat = new DefaultCSVFormat { override val quoting = QUOTE_NONE }
        val outFile = dest / (src.baseName + ".csv")
        val writer = CSVWriter.open(outFile.toIO)(csvFormat)
        val str = os.read(src)
        val json = ujson.read(str)

        writer.writeRow(List(
          "execution_time",
          "branch_name",
          "commit_ref",
          "commit_time",
          "benchmark_name",
          "benchmark_type",
          "compilation_mode",
          "measurement"
        ))

        json("results")(0)("times").arr.foreach { measurement =>
          writer.writeRow(List(
            json("results")(0)("executionTime").str,
            json("results")(0)("branchName").str,
            json("results")(0)("commitRef").str,
            json("results")(0)("commitTime").str,
            src.baseName,
            "duration",
            "command_line_interface",
            (measurement.num * 1000).toInt
          ))
        }

        writer.close()
      }

    mill.api.Result.Success(PathRef(dest))
  }

  def publishBenchmarkCsv() = T.command {
    val benchmarkCsvDir = benchmarkCsv()
    val env = T.ctx().env
    val benchmarkDbUrl = env("INC_BENCHMARK_DB_URL")
    val benchmarkDbUser = env("INC_BENCHMARK_DB_USER")
    val benchmarkDbPassword = env("INC_BENCHMARK_DB_PASSWORD")

    Class.forName("org.postgresql.Driver")
    val conn = DriverManager.getConnection(benchmarkDbUrl, benchmarkDbUser, benchmarkDbPassword)
    val copyManager = new CopyManager(conn.asInstanceOf[BaseConnection])

    if (benchmarkCsvDir.path.toIO.exists) {
      os.walk(benchmarkCsvDir.path)
        .filter(_.last.matches(".*.csv"))
        .foreach { csv =>
          T.ctx().log.info(s"Uploading benchmark data from $csv")
          val reader = new FileReader(csv.toIO)
          copyManager.copyIn("""copy benchmark_results from stdin with csv header quote '"'""", reader)
        }
    }
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
