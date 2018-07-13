package inc.main

case class Configuration(
  printParser: Boolean = false,
  printTyper: Boolean = false,
  printCodegen: Boolean = false,
  printPhaseTiming: Boolean = false
)

object Configuration {
  val default = Configuration()
  val printTimings = Configuration(printPhaseTiming = true)
}
