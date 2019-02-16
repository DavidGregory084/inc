package inc

import java.lang.{ String, System }
import scala.Some
import scribe._
import scribe.format._

package object common {
  Logger.root
    .clearHandlers()
    .withHandler(
      formatter = Formatter.simple,
      minimumLevel = Some(Level.Info)
    ).replace()

  val NL = System.lineSeparator
  def Green(str: String) = fansi.Color.Green(str).render
  def Yellow(str: String) = fansi.Color.Yellow(str).render
  def Red(str: String) = fansi.Color.Red(str).render
  def White(str: String) = fansi.Color.White(str).render
  def Blue(str: String) = fansi.Color.Blue(str).render
}
