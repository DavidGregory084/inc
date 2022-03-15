package inc.main

import org.typelevel.paiges._

import java.lang.String
import scala.Long
import scala.StringContext

object Messages {
  def compilationTime(fileName: String, start: Long, end: Long) = {
    val timeMillis   = (end - start) / 1000000
    val msgText      = Doc.text(s"Compiled ${fileName} in").style(Style.Ansi.Fg.Blue)
    val timingText   = Doc.text(s"${timeMillis}ms").style(Style.Ansi.Fg.White)
    val formattedMsg = Doc.hardLine + msgText & timingText
    formattedMsg.render(80)
  }

  def compilationErrorTime(fileName: String, start: Long, end: Long) = {
    val timeMillis   = (end - start) / 1000000
    val msgText      = Doc.text(s"Compiling ${fileName} failed after").style(Style.Ansi.Fg.Red)
    val timingText   = Doc.text(s"${timeMillis}ms").style(Style.Ansi.Fg.White)
    val formattedMsg = Doc.hardLine + msgText & timingText
    formattedMsg.render(80)
  }

  def phaseTime(phaseName: String, fileName: String, start: Long, end: Long) = {
    val timeMillis = (end - start) / 1000000
    val msgText = Doc.text(s"Completed ${phaseName} for ${fileName} in").style(Style.Ansi.Fg.Blue)
    val timingText   = Doc.text(s"${timeMillis}ms").style(Style.Ansi.Fg.White)
    val formattedMsg = Doc.hardLine + msgText & timingText
    formattedMsg.render(80)
  }
}
