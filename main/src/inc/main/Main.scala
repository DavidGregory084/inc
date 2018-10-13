package inc.main

import inc.common._
import inc.parser.Parser
import inc.resolver.Resolver
import inc.typechecker.Typechecker
import inc.codegen.Codegen

import better.files._
import cats.data.{ Chain, Validated }
import cats.instances.list._
import cats.syntax.traverse._
import java.net.URLClassLoader
import java.io.{File => JavaFile, ByteArrayOutputStream}
import java.net.URL
import java.nio.file.Paths

case class ConfigError(private val position: Pos, private val message: String) extends Error(position, message)

object Main {
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

    parser.parse(args, Configuration()).foreach { config =>
      compileProgram(dir, prog, config) match {
        case Left(errors) =>
          errors.map { e =>
            Printer.withSourceContext(Some("<stdin>"), e.getMessage, e.pos, fansi.Color.Red, prog)
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
    val urlStrings = classpath.split(JavaFile.pathSeparator)
    Chain.fromSeq(urlStrings).traverse { p =>
      val path = Validated.catchNonFatal(Paths.get(p))
      val url = path.map(_.toUri.toURL)
      url.leftMap(t => List(ConfigError(Pos.Empty, t.getMessage)))
    }.map(_.iterator.toArray).toEither
  }

  def compileProgram(dest: File, prog: String, config: Configuration = Configuration.default): Either[List[Error], File] = {
    val beforeAll = System.nanoTime

    for {
      urls <- parseUrls(config.classpath)

      mod <- runPhase[Module[Pos]]("parser", config, _.printParser, Parser.parse(prog))

      importedDecls = readEnvironment(mod.imports, new URLClassLoader(urls))

      resolved <- runPhase[Module[NameWithPos]]("resolver", config, _.printResolver, Resolver.resolve(mod, importedDecls))

      checked <- runPhase[Module[NamePosType]]("typechecker", config, _.printTyper, Typechecker.typecheck(resolved, importedDecls, prog))

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

      scribe.info(NL + Blue(s"""Compiled ${mod.pkg.mkString(".")}.${mod.name} in """) + White(s"""${(afterAll - beforeAll) / 1000000}ms"""))

      out
    }
  }
}
