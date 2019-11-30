package inc

import org.typelevel.paiges.Style
import java.lang.{ String, System }

package object common {
  val NL = System.lineSeparator

  def styled(style: Style, str: String) = {
    style.start + str + style.end
  }

  def Green(str: String) =
    styled(Style.Ansi.Fg.Green, str)

  def Yellow(str: String) =
    styled(Style.Ansi.Fg.Yellow, str)

  def Red(str: String) =
    styled(Style.Ansi.Fg.Red, str)

  def White(str: String) =
    styled(Style.Ansi.Fg.White, str)

  def Blue(str: String) =
    styled(Style.Ansi.Fg.Blue, str)
}
