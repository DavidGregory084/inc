package inc.main

import java.lang.String
import java.nio.file.{ Files, Paths }
import scala.{ StringContext, Unit }
import scala.util.control.NonFatal

object CmdParser extends scopt.OptionParser[Configuration]("inc") {
  var dir = Paths.get(".")
  var file = ""

  head("inc", Build.version)

  help("help")

  version("version")

  opt[String]("classpath")
    .abbr("cp")
    .text("The classpath to use when compiling. Defaults to the current directory.")
    .action((cp, config) => config.copy(classpath = cp))

  opt[String]("destination")
    .abbr("d")
    .text("The destination directory for compilation output. Defaults to the current directory.")
    .validate { d =>
      try {
        Paths.get(d)
        success
      } catch {
        case NonFatal(e) =>
          failure(s"The destination $d is not valid: ${e.getMessage}")
      }
    }.foreach { d =>
      dir = Paths.get(d)
    }

  opt[Unit]("trace-typer")
    .text("Print a trace during the typechecking phase")
    .action((_, config) => config.copy(traceTyper = true))

  opt[Unit]("print-parser")
    .text("Print syntax trees after the parsing phase")
    .action((_, config) => config.copy(printParser = true))

  opt[Unit]("print-resolver")
    .text("Print syntax trees after the name resolution phase")
    .action((_, config) => config.copy(printResolver = true))

  opt[Unit]("print-typer")
    .text("Print syntax trees after the type inference phase")
    .action((_, config) => config.copy(printTyper = true))

  opt[Unit]("print-codegen")
    .text("Print the java bytecode generated by the code generation phase")
    .action((_, config) => config.copy(printCodegen = true))

  opt[Unit]("print-timings")
    .text("Print the time taken in each phase")
    .action((_, config) => config.copy(printPhaseTiming = true))

  opt[Unit]("verify-codegen")
    .text("Run a verifier on the code produced in codegen")
    .action((_, config) => config.copy(verifyCodegen = true))

  arg[String]("<file>")
    .text("The source file to compile")
    .validate { f =>
      try {
        val path = Paths.get(f)
        if (Files.exists(path))
          success
        else
          failure(s"The file $f does not exist")
      } catch {
        case NonFatal(e) =>
          failure(s"The file $f is not valid: ${e.getMessage}")
      }
    }.foreach { f =>
      file = f
    }
}