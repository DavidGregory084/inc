package inc.main

import inc.parser.Parser
import inc.codegen.Codegen

import ammonite.ops._

object Main {
  def compileProgram(dest: Path, prog: String): Path = {
    val mod = Parser.parse(prog)
    val code = Codegen.generate(mod)

    val packageNameSegments = {
      val nameOrEmpty = mod.pkg.getOrElse("")
      val nameSegments = nameOrEmpty.split("\\.")
      nameSegments.filterNot(_.isEmpty)
    }

    val outDir = packageNameSegments.foldLeft(dest) {
      case (path, next) => path / next
    }

    val out = outDir / s"${mod.name}.class"

    rm! out
    write(out, code)

    out
  }
}
