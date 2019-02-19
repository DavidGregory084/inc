package inc

import java.lang.{ String, System }

package object common {
  val NL = System.lineSeparator
  def Green(str: String) = fansi.Color.Green(str).render
  def Yellow(str: String) = fansi.Color.Yellow(str).render
  def Red(str: String) = fansi.Color.Red(str).render
  def White(str: String) = fansi.Color.White(str).render
  def Blue(str: String) = fansi.Color.Blue(str).render
}
