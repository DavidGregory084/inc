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
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.{ Array, Boolean, Byte, Long, Unit, Either, Option, Some, StringContext }
import scala.collection.JavaConverters._
import scala.collection.immutable.{ List, Map }
import scala.Predef.{ ArrowAssoc, wrapRefArray }
import scala.util.control.NonFatal
import scribe._
import scribe.format._

case class ConfigError(private val position: Pos, private val message: String) extends Error(position, message)

object Main {
  def main(args: Array[String]): Unit = {
    Logger.root.clearHandlers().withHandler(
      formatter = Formatter.simple,
      minimumLevel = Some(Level.Info)
    ).replace()

    var dir = Paths.get(".")
    var file = ""

    val parser = new scopt.OptionParser[Configuration]("inc") {
      head("inc", Build.version)

      help("help")

      version("version")

      opt[String]("classpath")
        .abbr("cp")
        .text("The classpath to use when compiling. Defaults to the current directory.")
        .action((cp, config) => config.copy(classpath = cp))

      opt[String]("destination")
        .abbr("d")
        .text("The destination directory for compilation output. Defaults to the current directory.")
        .validate { d =>
          try {
            Paths.get(d)
            success
          } catch {
            case NonFatal(e) =>
              failure(s"The destination $d is not valid: ${e.getMessage}")
          }
        }.foreach { d =>
          dir = Paths.get(d)
        }

      opt[Unit]("trace-typer")
        .text("Print a trace during the typechecking phase")
        .action((_, config) => config.copy(traceTyper = true))

      opt[Unit]("print-parser")
        .text("Print syntax trees after the parsing phase")
        .action((_, config) => config.copy(printParser = true))

      opt[Unit]("print-resolver")
        .text("Print syntax trees after the name resolution phase")
        .action((_, config) => config.copy(printResolver = true))

      opt[Unit]("print-typer")
        .text("Print syntax trees after the type inference phase")
        .action((_, config) => config.copy(printTyper = true))

      opt[Unit]("print-codegen")
        .text("Print the java bytecode generated by the code generation phase")
        .action((_, config) => config.copy(printCodegen = true))

      opt[Unit]("print-timings")
        .text("Print the time taken in each phase")
        .action((_, config) => config.copy(printPhaseTiming = true))

      opt[Unit]("verify-codegen")
        .text("Run a verifier on the code produced in codegen")
        .action((_, config) => config.copy(verifyCodegen = true))

      arg[String]("<file>")
        .text("The source file to compile")
        .validate { f =>
          try {
            val path = Paths.get(f)
            if (Files.exists(path))
              success
            else
              failure(s"The file $f does not exist")
          } catch {
            case NonFatal(e) =>
              failure(s"The file $f is not valid: ${e.getMessage}")
          }
        }.foreach { f =>
          file = f
        }
    }

    parser.parse(args, Configuration()).foreach { config =>
      val path = Paths.get(file)
      val mod = readFileAsString(path)
      val fileName = dir.resolve(path).toString
      compileModule(dir, mod, config).left.foreach { errors =>
        errors.map { e =>
          Trace.withSourceContext(Some(fileName), e.getMessage, e.pos, fansi.Color.Red, mod)
        }.foreach(scribe.error(_))
      }
    }
  }

  def readFileAsString(path: Path) = Files
    .readAllLines(path, StandardCharsets.UTF_8)
    .asScala.mkString(System.lineSeparator)

  def printPhaseTiming(phase: String, before: Long, after: Long): Unit =
    scribe.info(NL + Blue(s"""Completed ${phase} in """) + White(s"""${(after - before) / 1000000}ms"""))

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

    val typechecker = new Typechecker(config.traceTyper)
    val codegen = new Codegen(config.verifyCodegen)

    val res = for {
      urls <- parseUrls(config.classpath)

      mod <- runPhase[Module[Pos]]("parser", config, _.printParser, Parser.parse(modSource))

      importedDecls = readEnvironment(mod.imports, new URLClassLoader(urls))

      resolved <- runPhase[Module[NameWithPos]]("resolver", config, _.printResolver, Resolver.resolve(mod, importedDecls))

      checked <- runPhase[Module[NamePosType]]("typechecker", config, _.printTyper, typechecker.typecheck(resolved, importedDecls, modSource))

      code <- runPhase[Array[Byte]]("codegen", config, _.printCodegen, codegen.generate(checked), codegen.print(_))

    } yield {
      val outDir = mod.pkg.foldLeft(dest) {
        case (path, next) => path.resolve(next)
      }

      val out = outDir.resolve(s"${mod.name}.class")

      Files.deleteIfExists(out)

      Files.createDirectories(out.getParent)

      Files.write(out, code)

      val afterAll = System.nanoTime

      val name = if (mod.pkg.isEmpty) mod.name else mod.pkg.mkString(".") + "." + mod.name

      scribe.info(NL + Blue(s"""Compiled ${name} in """) + White(s"""${(afterAll - beforeAll) / 1000000}ms"""))

      out
    }

    res.left.foreach { _ =>
      val afterAll = System.nanoTime
      scribe.info(NL + Red(s"""Compilation failed after """) + White(s"""${(afterAll - beforeAll) / 1000000}ms"""))
    }

    res
  }
}
