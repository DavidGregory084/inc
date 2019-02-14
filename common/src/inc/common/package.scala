package inc

import java.lang.{ String, System }
import qq.droste.data.Fix
import scribe._
import scribe.format._

package object common {
  type ExprF[A] = Fix[Expr[A, ?]]

  Logger.root
    .clearHandlers()
    .withHandler(formatter = Formatter.simple)
    .replace()

  val NL = System.lineSeparator
  def Green(str: String) = fansi.Color.Green(str).render
  def Yellow(str: String) = fansi.Color.Yellow(str).render
  def Red(str: String) = fansi.Color.Red(str).render
  def White(str: String) = fansi.Color.White(str).render
  def Blue(str: String) = fansi.Color.Blue(str).render
}
