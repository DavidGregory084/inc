package inc.main

import inc.common.Error
import inc.parser.Parser
import inc.typechecker.Typechecker
import inc.codegen.Codegen

import ammonite.ops._

object Main {
  def compileProgram(dest: Path, prog: String): Either[List[Error], Path] = {
    for {
      mod <- Parser.parse(prog)
      // _ = pprint.pprintln(mod)
      checked <- Typechecker.typecheck(mod)
      _ = pprint.pprintln(checked)
      code <- Codegen.generate(checked)
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
