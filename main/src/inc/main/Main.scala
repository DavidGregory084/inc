package inc.main

import inc.common._
import inc.parser.Parser
import inc.resolver.Resolver
import inc.typechecker.Typechecker
import inc.codegen.Codegen
import cats.data.{Chain, Validated}
import cats.instances.list._
import cats.syntax.traverse._
import java.lang.{ ClassLoader, String, System }
import java.io.{ByteArrayOutputStream, File, InputStream, OutputStream}
import java.net.{ URL, URLClassLoader }
import java.nio.file.{Files, Path, Paths}
import scala.{ Array, Boolean, Byte, Long, Unit, Either, Left, Right, Option, Some, StringContext }
import scala.collection.immutable.{ List, Map }
import scala.Predef.{ ArrowAssoc, wrapRefArray }

case class ConfigError(private val position: Pos, private val message: String) extends Error(position, message)

object Main {
  def main(args: Array[String]): Unit = {
    val dir = Paths.get(".")

    var mod = ""

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

      arg[String]("<module source>").action{ (m, config) =>
        mod = m
        config
      }
    }

    parser.parse(args, Configuration()).foreach { config =>
      compileModule(dir, mod, config) match {
        case Left(errors) =>
          errors.map { e =>
            Printer.withSourceContext(Some("<stdin>"), e.getMessage, e.pos, fansi.Color.Red, mod)
          }.foreach(scribe.error(_))
          scribe.info(NL + Red("Failure") + NL)
        case Right(_) =>
          scribe.info(NL + Green("Success") + NL)
      }
    }
  }

  def printPhaseTiming(phase: String, before: Long, after: Long): Unit =
    scribe.info(NL + s"Completed $phase in ${(after - before) / 1000000}ms")

  def runPhase[A](
    name: String,
    config: Configuration,
    printOutput: Configuration => Boolean,
    phase: => Either[List[Error], A],
    print: A => Unit = (a: A) => {
      scribe.info(NL + pprint.apply(a, height = 1000))
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

  def readClassBytes(classloader: ClassLoader, className: String): Array[Byte] = {
    val classStream = Option(classloader.getResourceAsStream(className))
    val outputStream = new ByteArrayOutputStream()

    val bufSize = 8192
    val buf = new Array[Byte](bufSize)

    def pipe(is: InputStream, os: OutputStream, buf: Array[Byte]): Unit = {
      val num = is.read(buf)
      if (num > 0) {
        os.write(buf)
        pipe(is, os, buf)
      }
    }

    classStream.foreach { inputStream =>
      try pipe(inputStream, outputStream, buf)
      finally {
        inputStream.close()
        outputStream.close()
      }
    }

    outputStream.toByteArray
  }

  def readEnvironment(imports: List[Import], classloader: ClassLoader): Map[String, TopLevelDeclaration[NameWithType]] = {
    val distinctPrefixes = imports.map {
      case ImportModule(pkg, nm) =>
        (pkg, nm, List.empty[String])
      case ImportSymbols(pkg, nm, syms) =>
        (pkg, nm, syms)
    }.distinct

    val importedDecls = distinctPrefixes.flatMap {
      case (pkg, nm, syms) =>
        val className = pkg.mkString("/") + "/" + nm + ".class"
        val classBytes = readClassBytes(classloader, className)
        val maybeInterface = Codegen.readInterface(classBytes)

        maybeInterface
          .toList
          .flatMap { mod =>
            val decls =
              if (syms.isEmpty)
                mod.declarations
              else
                mod.declarations.filter(d => syms.contains(d.name))

            decls.map(d => d.name -> d)
          }
    }

    importedDecls.toMap
  }

  def parseUrls(classpath: String): Either[List[Error], Array[URL]] = {
    val urlStrings = classpath.split(File.pathSeparator)
    Chain.fromSeq(urlStrings).traverse { p =>
      val path = Validated.catchNonFatal(Paths.get(p))
      val url = path.map(_.toUri.toURL)
      url.leftMap(t => List(ConfigError(Pos.Empty, t.getMessage)))
    }.map(_.iterator.toArray).toEither
  }

  def compileModule(dest: Path, modSource: String, config: Configuration = Configuration.default): Either[List[Error], Path] = {
    val beforeAll = System.nanoTime

    for {
      urls <- parseUrls(config.classpath)

      mod <- runPhase[Module[Pos]]("parser", config, _.printParser, Parser.parse(modSource))

      importedDecls = readEnvironment(mod.imports, new URLClassLoader(urls))

      resolved <- runPhase[Module[NameWithPos]]("resolver", config, _.printResolver, Resolver.resolve(mod, importedDecls))

      checked <- runPhase[Module[NamePosType]]("typechecker", config, _.printTyper, Typechecker.typecheck(resolved, importedDecls, modSource))

      code <- runPhase[Array[Byte]]("codegen", config, _.printCodegen, Codegen.generate(checked), Codegen.print(_))

    } yield {
      val outDir = mod.pkg.foldLeft(dest) {
        case (path, next) => path.resolve(next)
      }

      val out = outDir.resolve(s"${mod.name}.class")

      Files.deleteIfExists(out)

      Files.createDirectories(out.getParent)

      Files.write(out, code)

      val afterAll = System.nanoTime

      scribe.info(NL + Blue(s"""Compiled ${mod.pkg.mkString(".")}.${mod.name} in """) + White(s"""${(afterAll - beforeAll) / 1000000}ms"""))

      out
    }
  }
}
