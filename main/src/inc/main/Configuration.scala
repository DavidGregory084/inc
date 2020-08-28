package inc.main

import java.lang.String
import scala.{ Boolean, Option, None }

case class Configuration(
  classpath: String = ".",
  traceTyper: Boolean = false,
  printParser: Boolean = false,
  printResolver: Boolean = false,
  printTyper: Boolean = false,
  printCodegen: Boolean = false,
  printPhaseTiming: Boolean = false,
  verifyCodegen: Boolean = false,
  exitOnError: Boolean = true,
  stopBefore: Option[Phase] = None
)

object Configuration {
  val default = Configuration()
  val test = Configuration(verifyCodegen = true, exitOnError = false)
  val printTimings = Configuration(printPhaseTiming = true)
}
