package inc.main

case class Configuration(
  classpath: String = ".",
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
