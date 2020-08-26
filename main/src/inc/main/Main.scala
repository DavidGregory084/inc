package inc.main

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

      compileModule(dir, fileName, config, source).left.foreach { errors =>
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
    phaseName: String,
    fileName: String,
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
        logger.info(Messages.phaseTime(phaseName, fileName, before, after))
      }

      _ = if (printOutput(config)) print(out)

    } yield out
  }

  def compileModule(dest: Path, fileName: String, config: Configuration = Configuration.default, source: String): Either[List[Error], List[Path]] = {
    val beforeAll = System.nanoTime

    val codegen = new Codegen(config.verifyCodegen)

    val res = for {
      urls <- Classpath.parseUrls(config.classpath)

      mod <- runPhase[Module[Pos]]("parser", fileName, config, _.printParser, Parser.parse(source))

      importedEnv <- Classpath.readEnvironment(mod.imports, new URLClassLoader(urls))

      resolved <- runPhase[Module[Meta.Untyped]]("resolver", fileName, config, _.printResolver, Resolver.resolve(mod, importedEnv.forgetMemberTypes))

      checked <- runPhase[Module[Meta.Typed]]("typechecker", fileName, config, _.printTyper, Typechecker.typecheck(resolved, importedEnv))

      classFiles <- runPhase[List[ClassFile]]("codegen", fileName, config, _.printCodegen, codegen.generate(checked, importedEnv), _.foreach(f => codegen.print(f.bytes)))

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

    res.left.foreach { _ =>
      val afterAll = System.nanoTime
      logger.info(Messages.compilationErrorTime(fileName, beforeAll, afterAll))
    }

    res
  }
}
