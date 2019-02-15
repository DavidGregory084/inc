package inc.main

import java.lang.String
import scala.Boolean

case class Configuration(
  classpath: String = ".",
  traceTyper: Boolean = false,
  printParser: Boolean = false,
  printResolver: Boolean = false,
  printTyper: Boolean = false,
  printCodegen: Boolean = false,
  printPhaseTiming: Boolean = false
)

object Configuration {
  val default = Configuration()
  val printTimings = Configuration(printPhaseTiming = true)
}
