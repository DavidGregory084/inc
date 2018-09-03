package inc.main

import inc.common._
import inc.parser.Parser
import inc.resolver.Resolver
import inc.typechecker.Typechecker
import inc.codegen.Codegen

import better.files._

object Main {
  def main(args: Array[String]): Unit = {
    val dir = File.newTemporaryDirectory()

    val result = try {
      compileProgram(dir, args(0), Configuration.printTimings)
    } finally {
      dir.delete()
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

  def compileProgram(dest: File, prog: String, config: Configuration = Configuration.default): Either[List[Error], File] = {
    val beforeAll = System.nanoTime

    for {

      mod <- runPhase[Module[Unit]]("parser", config, _.printParser, Parser.parse(prog))

      resolved <- runPhase[Module[Name]]("resolver", config, _.printResolver, Resolver.resolve(mod))

      checked <- runPhase[Module[NameWithType]]("typechecker", config, _.printTyper, Typechecker.typecheck(resolved))

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

      println(s"""Compiled ${mod.pkg.mkString(".")}.${mod.name} in ${(afterAll - beforeAll) / 1000000}ms""")

      out
    }
  }
}
