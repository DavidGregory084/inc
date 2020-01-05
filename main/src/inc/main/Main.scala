package inc.main

import inc.common._
import inc.parser.Parser
import inc.resolver.Resolver
import inc.typechecker.Typechecker
import inc.codegen.Codegen
import com.typesafe.scalalogging.LazyLogging
import java.lang.{ String, System }
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import org.jline.terminal.TerminalBuilder
import org.typelevel.paiges.Style
import scala.{ Array, Boolean, Byte, Unit, Either, StringContext }
import scala.collection.immutable.List
import scala.jdk.CollectionConverters._
import scala.Predef.wrapRefArray

object Main extends LazyLogging {
  def main(args: Array[String]): Unit = {
    val terminal = TerminalBuilder.terminal

    def highlightError(context: Printer.SourceContext, msg: String, pos: Pos): String =
      Printer.withSourceContext(context)(msg, pos, Style.Ansi.Fg.Red)

    CmdParser.parse(args, Configuration()).foreach { config =>
      val dir = CmdParser.dir
      val file = CmdParser.file
      val path = Paths.get(file)
      val source = readFileAsString(path)
      val fileName = dir.resolve(path).toString
      val context = Printer.SourceContext(terminal.getWidth, fileName, source)

      compileModule(dir, context, config).left.foreach { errors =>
        errors.map { e =>
          highlightError(context, NL + e.getMessage, e.pos)
        }.foreach(logger.error(_))

        if (config.exitOnError)
          System.exit(1)
      }
    }
  }

  def readFileAsString(path: Path) = Files
    .readAllLines(path, StandardCharsets.UTF_8)
    .asScala.mkString(System.lineSeparator)

  def runPhase[A](
    name: String,
    context: Printer.SourceContext,
    config: Configuration,
    printOutput: Configuration => Boolean,
    phase: => Either[List[Error], A],
    print: A => Unit = (a: A) => {
      logger.info(NL + pprint.apply(a, height = 1000))
    }
  ): Either[List[Error], A] = {
    val before = System.nanoTime

    for {
      out <- phase

      after = System.nanoTime

      _ = if (config.printPhaseTiming) {
        logger.info(Messages.phaseTime(context, name, before, after))
      }

      _ = if (printOutput(config)) print(out)

    } yield out
  }

  def compileModule(dest: Path, context: Printer.SourceContext, config: Configuration = Configuration.default): Either[List[Error], Path] = {
    val beforeAll = System.nanoTime

    val typechecker = new Typechecker(config.traceTyper)
    val codegen = new Codegen(config.verifyCodegen)

    val res = for {
      urls <- Classpath.parseUrls(config.classpath)

      mod <- runPhase[Module[Pos]]("parser", context, config, _.printParser, Parser.parse(context.source))

      importedEnv <- Classpath.readEnvironment(mod.imports, new URLClassLoader(urls))

      resolved <- runPhase[Module[Meta.Untyped]]("resolver", context, config, _.printResolver, Resolver.resolve(mod, importedEnv))

      checked <- runPhase[Module[Meta.Typed]]("typechecker", context, config, _.printTyper, typechecker.typecheck(resolved, importedEnv, context))

      code <- runPhase[Array[Byte]]("codegen", context, config, _.printCodegen, codegen.generate(checked), codegen.print(_))

    } yield {
      val outDir = mod.pkg.foldLeft(dest) {
        case (path, next) => path.resolve(next)
      }

      val out = outDir.resolve(s"${mod.name}.class")

      Files.deleteIfExists(out)

      Files.createDirectories(out.getParent)

      Files.write(out, code)

      val afterAll = System.nanoTime

      logger.info(Messages.compilationTime(context, beforeAll, afterAll))

      out
    }

    res.left.foreach { _ =>
      val afterAll = System.nanoTime
      logger.info(Messages.compilationErrorTime(context, beforeAll, afterAll))
    }

    res
  }
}
