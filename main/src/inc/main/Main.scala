package inc.main

import inc.common._
import inc.parser.Parser
import inc.typechecker.Typechecker
import inc.codegen.Codegen

import ammonite.ops._

object Main {
  def main(args: Array[String]): Unit = {
    val dir = tmp.dir()

    val result = try {
      compileProgram(dir, args(0), Configuration.printTimings)
    } finally {
      rm! dir
    }

    result match {
      case Left(errors) =>
        errors.foreach(println)
      case Right(_) =>
        println("Success")
    }
  }

  def printPhaseTiming(phase: String, before: Long, after: Long): Unit =
    println(s"Completed $phase in ${(after - before) / 1000000}ms")

  def runPhase[A](
    name: String,
    config: Configuration,
    printOutput: Configuration => Boolean,
    phase: => Either[List[Error], A],
    print: A => Unit = (a: A) => pprint.pprintln(a)
  ): Either[List[Error], A] = {
    val before = System.nanoTime

    for {
      out <- phase

      after = System.nanoTime

      _ = if (config.printPhaseTiming) printPhaseTiming(name, before, after)

      _ = if (printOutput(config)) print(out)

    } yield out
  }

  def compileProgram(dest: Path, prog: String, config: Configuration = Configuration.default): Either[List[Error], Path] = {
    val beforeAll = System.nanoTime

    for {

      mod <- runPhase[Module[Unit]]("parser", config, _.printParser, Parser.parse(prog))

      checked <- runPhase[Module[Type]]("typechecker", config, _.printTyper, Typechecker.typecheck(mod))

      code <- runPhase[Array[Byte]]("codegen", config, _.printCodegen, Codegen.generate(checked), Codegen.print)

    } yield {
      val outDir = mod.pkg.foldLeft(dest) {
        case (path, next) => path / next
      }

      val out = outDir / s"${mod.name}.class"

      rm! out
      write(out, code)

      val afterAll = System.nanoTime

      println(s"""Compiled ${mod.pkg.mkString(".")}.${mod.name} in ${(afterAll - beforeAll) / 1000000}ms""")

      out
    }
  }
}
