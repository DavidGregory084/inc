package inc.main

import inc.common._
import inc.parser.Parser
import inc.resolver.Resolver
import inc.typechecker.Typechecker
import inc.codegen.Codegen

import better.files._
import cats.data.Chain
import cats.data.Validated
import cats.implicits._
import java.net.URLClassLoader
import java.io.{File => JavaFile, ByteArrayOutputStream}
import java.net.URL
import java.nio.file.Paths

object Main {
  val NL = System.lineSeparator

  def main(args: Array[String]): Unit = {
    val dir = ".".toFile

    var prog = ""

    val parser = new scopt.OptionParser[Configuration]("inc") {
      head("inc", "0.1.0-SNAPSHOT")

      opt[String]("classpath")
        .abbr("cp")
        .action((cp, config) => config.copy(classpath = cp))

      opt[Unit]("print-parser")
        .action((_, config) => config.copy(printParser = true))

      opt[Unit]("print-resolver")
        .action((_, config) => config.copy(printResolver = true))

      opt[Unit]("print-typer")
        .action((_, config) => config.copy(printTyper = true))

      opt[Unit]("print-codegen")
        .action((_, config) => config.copy(printCodegen = true))

      opt[Unit]("print-timings")
        .action((_, config) => config.copy(printPhaseTiming = true))

      arg[String]("<prog>").action{ (p, config) =>
        prog = p
        config
      }
    }

    parser.parse(args, Configuration()) foreach { config =>
      compileProgram(dir, prog, config) match {
        case Left(errors) =>
          errors.map(e => NL + e.getMessage).foreach(println)
        case Right(_) =>
          println(NL + "Success")
      }
    }
  }

  def printPhaseTiming(phase: String, before: Long, after: Long): Unit =
    println(NL + s"Completed $phase in ${(after - before) / 1000000}ms")

  def runPhase[A](
    name: String,
    config: Configuration,
    printOutput: Configuration => Boolean,
    phase: => Either[List[Error], A],
    print: A => Unit = (a: A) => {
      println()
      pprint.pprintln(a)
    }
  ): Either[List[Error], A] = {
    val before = System.nanoTime

    for {
      out <- phase

      after = System.nanoTime

      _ = if (config.printPhaseTiming) {
        printPhaseTiming(name, before, after)
      }

      _ = if (printOutput(config)) print(out)

    } yield out
  }

  def readImports(imports: List[Import], classloader: ClassLoader): Map[(List[String], String), Module[NameWithType]] = {
    val distinctPrefixes = imports.map {
      case ImportModule(pkg, nm) =>
        (pkg, nm)
      case ImportSymbols(pkg, nm, _) =>
        (pkg, nm)
    }.distinct

    val modules = distinctPrefixes.flatMap {
      case (pkg, nm) =>
        val className = pkg.mkString("/") + "/" + nm + ".class"
        val classStream = Option(classloader.getResourceAsStream(className))
        val outputStream = new ByteArrayOutputStream()

        classStream.foreach { inputStream =>
          for {
            in <- inputStream.autoClosed
            out <- outputStream.autoClosed
          } in.pipeTo(out)
        }

        val maybeInterface = Codegen.readInterface(outputStream.toByteArray)

        maybeInterface
          .toList
          .map { mod => (pkg, nm) -> mod }
    }

    modules.toMap
  }

  def parseUrls(classpath: String): Either[List[Throwable], Array[URL]] = {
    val urlStrings = classpath.split(JavaFile.pathSeparator)
    Chain.fromSeq(urlStrings).traverse { p =>
      val path = Validated.catchNonFatal(Paths.get(p))
      val url = path.map(_.toUri.toURL)
      url.leftMap(t => List(t))
    }.map(_.iterator.toArray).toEither
  }

  def compileProgram(dest: File, prog: String, config: Configuration = Configuration.default): Either[List[Throwable], File] = {
    val beforeAll = System.nanoTime

    for {
      urls <- parseUrls(config.classpath)

      mod <- runPhase[Module[Unit]]("parser", config, _.printParser, Parser.parse(prog))

      _ = println(NL + mod)

      importedMods = readImports(mod.imports, new URLClassLoader(urls))

      resolved <- runPhase[Module[Name]]("resolver", config, _.printResolver, Resolver.resolve(mod, importedMods))

      checked <- runPhase[Module[NameWithType]]("typechecker", config, _.printTyper, Typechecker.typecheck(resolved, importedMods))

      code <- runPhase[Array[Byte]]("codegen", config, _.printCodegen, Codegen.generate(checked), Codegen.print(_))

    } yield {
      val outDir = mod.pkg.foldLeft(dest) {
        case (path, next) => path / next
      }

      val out = outDir / s"${mod.name}.class"

      if (out.exists)
        out.delete()

      out
        .createIfNotExists(createParents = true)
        .writeByteArray(code)

      val afterAll = System.nanoTime

      println(NL + s"""Compiled ${mod.pkg.mkString(".")}.${mod.name} in ${(afterAll - beforeAll) / 1000000}ms""")

      out
    }
  }
}
