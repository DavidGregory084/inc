package inc.main

import inc.common._
import inc.parser.Parser
import inc.resolver.Resolver
import inc.typechecker.Typechecker
import inc.codegen.Codegen

import better.files._
import cats.data.{ Chain, Validated }
import cats.implicits._
import java.net.URLClassLoader
import java.io.{File => JavaFile, ByteArrayOutputStream}
import java.net.URL
import java.nio.file.Paths
import scribe._
import scribe.format._

case class ConfigError(private val position: Pos, private val message: String) extends Error(position, message)

object Main {

  Logger.root
    .clearHandlers()
    .withHandler(formatter = Formatter.simple)
    .replace()

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

    def sourceContext(msg: String, prog: String, pos: Pos) = {
      val highlighted =
        if (pos.from > 0 && pos.to > pos.from)
          fansi.Str(prog).overlay(fansi.Color.Red, pos.from, pos.to)
        else
          fansi.Str(prog)

      val (highlightedLines, _) = highlighted.render.split('\n').foldLeft(Chain.empty[String] -> 0) {
        case ((lines, idx), line) =>
          val nextIdx = idx + 1 + fansi.Str(line).length
          (lines :+ (idx.toString.padTo(String.valueOf(highlighted.length).length + 1, ' ') + '|' + line), nextIdx)
      }

      NL + msg + ":" + NL + NL + highlightedLines.toList.mkString(System.lineSeparator)
    }

    parser.parse(args, Configuration()) foreach { config =>
      compileProgram(dir, prog, config) match {
        case Left(errors) =>
          errors.map { e =>
            sourceContext(e.getMessage, prog, e.pos)
          }.foreach(scribe.error(_))
        case Right(_) =>
          scribe.info(NL + "Success")
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
      scribe.info(NL + pprint.apply(a))
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

      checked <- runPhase[Module[NamePosType]]("typechecker", config, _.printTyper, Typechecker.typecheck(resolved, importedDecls))

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

      scribe.info(NL + s"""Compiled ${mod.pkg.mkString(".")}.${mod.name} in ${(afterAll - beforeAll) / 1000000}ms""")

      out
    }
  }
}
