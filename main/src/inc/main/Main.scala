package inc.main

import cats.data.OptionT
import cats.instances.either._
import cats.syntax.flatMap._
import inc.common._
import inc.parser.Parser
import inc.resolver.Resolver
import inc.typechecker.Typechecker
import inc.codegen.{ Codegen, ClassFile }
import com.typesafe.scalalogging.LazyLogging
import java.lang.{ String, System }
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.{ Array, Boolean, Unit, Either }
import scala.collection.immutable.List
import scala.jdk.CollectionConverters._
import scala.Predef.wrapRefArray

object Main extends LazyLogging {
  def main(args: Array[String]): Unit = {
    CmdParser.parse(args, Configuration()).foreach { config =>
      val dir = CmdParser.dir
      val file = CmdParser.file
      val path = Paths.get(file)
      val source = readFileAsString(path)
      val fileName = dir.resolve(path).toString

      compileModule(dir, fileName, config, source).value.left.foreach { errors =>
        errors
          .map(_.message)
          .foreach(logger.error(_))

        if (config.exitOnError)
          System.exit(1)
      }
    }
  }

  def readFileAsString(path: Path) = Files
    .readAllLines(path, StandardCharsets.UTF_8)
    .asScala.mkString(System.lineSeparator)

  def runPhase[A](
    phaseName: Phase,
    fileName: String,
    config: Configuration,
    printOutput: Configuration => Boolean,
    phase: => Either[List[Error], A],
    print: A => Unit = (a: A) => {
      logger.info(NL + pprint.apply(a, height = 1000))
    }
  ): Compile[A] = {
    val before = System.nanoTime
    val shouldRun = config.stopBefore.map(_ != phaseName).getOrElse(true)
    val shouldRunF = OptionT.pure[Either[List[Error], *]](shouldRun)

    for {
      out <- shouldRunF.ifM(OptionT.liftF(phase), OptionT.none)

      after = System.nanoTime

      _ = if (config.printPhaseTiming) {
        logger.info(Messages.phaseTime(phaseName.name, fileName, before, after))
      }

      _ = if (printOutput(config)) print(out)

    } yield out
  }
  
  def compileModule(dest: Path, fileName: String, config: Configuration = Configuration.default, source: String): Compile[List[Path]] = {
    val beforeAll = System.nanoTime

    val codegen = new Codegen(config.verifyCodegen)

    val res = for {
      urls <- Classpath.parseUrls(config.classpath)

      mod <- runPhase[Module[Pos]](Phase.Parser, fileName, config, _.printParser, Parser.parse(source))

      importedEnv <- Classpath.readEnvironment(mod.imports, new URLClassLoader(urls))

      resolved <- runPhase[Module[Meta.Untyped]](Phase.Resolver, fileName, config, _.printResolver, Resolver.resolve(mod, importedEnv.forgetMemberTypes))

      checked <- runPhase[Module[Meta.Typed]](Phase.Typer, fileName, config, _.printTyper, Typechecker.typecheck(resolved, importedEnv))

      classFiles <- runPhase[List[ClassFile]](Phase.Codegen, fileName, config, _.printCodegen, codegen.generate(checked, importedEnv), _.foreach(f => codegen.print(f.bytes)))

    } yield {
      val outDir = mod.pkg.foldLeft(dest) {
        case (path, next) => path.resolve(next)
      }

      val outFiles = classFiles.map { classFile =>
        val outFile = outDir.resolve(classFile.name + ".class")
        Files.deleteIfExists(outFile)
        Files.createDirectories(outFile.getParent)
        Files.write(outFile, classFile.bytes)
        outFile
      }

      val afterAll = System.nanoTime

      logger.info(Messages.compilationTime(fileName, beforeAll, afterAll))

      outFiles
    }

    res.value.left.foreach { _ =>
      val afterAll = System.nanoTime
      logger.info(Messages.compilationErrorTime(fileName, beforeAll, afterAll))
    }

    res
  }
}
