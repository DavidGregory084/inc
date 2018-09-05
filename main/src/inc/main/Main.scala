package inc.main

import inc.common._
import inc.parser.Parser
import inc.resolver.Resolver
import inc.typechecker.Typechecker
import inc.codegen.Codegen

import better.files._
import java.net.URLClassLoader
import java.io.{File => JavaFile}
import java.nio.file.Paths

object Main {
  def main(args: Array[String]): Unit = {
    println(java.nio.file.Paths.get("/home/david/Repos/inc/out/").toUri.toURL)

    // val cl = getClass.getClassLoader.asInstanceOf[java.net.URLClassLoader]
    // println(cl.getURLs.mkString(System.lineSeparator))

    // val dir = File.newTemporaryDirectory()
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

    // try {
      parser.parse(args, Configuration()) foreach { config =>
        val result = compileProgram(dir, prog, config)

        result match {
          case Left(errors) =>
            println()
            errors.foreach(println)
          case Right(_) =>
            println()
            println("Success")
        }
      }
    // } finally {
    //   dir.delete()
    // }
  }

  def printPhaseTiming(phase: String, before: Long, after: Long): Unit =
    println(s"Completed $phase in ${(after - before) / 1000000}ms")

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
        println()
        printPhaseTiming(name, before, after)
      }

      _ = if (printOutput(config)) print(out)

    } yield out
  }

  def compileProgram(dest: File, prog: String, config: Configuration = Configuration.default): Either[List[Error], File] = {
    val beforeAll = System.nanoTime

    val urls = config.classpath.split(JavaFile.pathSeparator).map(p => Paths.get(p).toUri.toURL)
    val classloader = new URLClassLoader(urls)

    for {

      mod <- runPhase[Module[Unit]]("parser", config, _.printParser, Parser.parse(prog))

      resolved <- runPhase[Module[Name]]("resolver", config, _.printResolver, Resolver.resolve(mod, classloader))

      checked <- runPhase[Module[NameWithType]]("typechecker", config, _.printTyper, Typechecker.typecheck(resolved, classloader))

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

      println()
      println(s"""Compiled ${mod.pkg.mkString(".")}.${mod.name} in ${(afterAll - beforeAll) / 1000000}ms""")

      out
    }
  }
}
