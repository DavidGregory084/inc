package inc.main

import inc.common.Error
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

  def compileProgram(dest: Path, prog: String, config: Configuration = Configuration.default): Either[List[Error], Path] = {
    val beforeAll = System.nanoTime

    for {
      mod <- Parser.parse(prog)

      afterParser = System.nanoTime

      _ = if (config.printPhaseTiming) printPhaseTiming("parser", beforeAll, afterParser)

      _ = if (config.printParser) pprint.pprintln(mod)

      beforeTyper = System.nanoTime

      checked <- Typechecker.typecheck(mod)

      afterTyper = System.nanoTime

      _ = if (config.printPhaseTiming) printPhaseTiming("typer", beforeTyper, afterTyper)

      _ = if (config.printTyper) pprint.pprintln(checked)

      beforeCodegen = System.nanoTime

      code <- Codegen.generate(checked)

      afterCodegen = System.nanoTime

      _ = if (config.printPhaseTiming) printPhaseTiming("codegen", beforeCodegen, afterCodegen)

      _ = if (config.printCodegen) Codegen.print(code)

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
