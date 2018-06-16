package inc.main

import inc.common.Error
import inc.parser.Parser
import inc.codegen.Codegen

import ammonite.ops._

object Main {
  def compileProgram(dest: Path, prog: String): Either[Error, Path] = {
    for {
      mod <- Parser.parse(prog)
      code <- Codegen.generate(mod)
    } yield {
      val outDir = mod.pkg.foldLeft(dest) {
        case (path, next) => path / next
      }

      val out = outDir / s"${mod.name}.class"

      rm! out
      write(out, code)

      out
    }
  }
}
