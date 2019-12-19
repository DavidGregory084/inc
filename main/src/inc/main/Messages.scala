package inc.main

import inc.common._
import java.lang.String
import org.typelevel.paiges._
import scala.{ Long, StringContext }

object Messages {
  def compilationTime(context: Printer.SourceContext, start: Long, end: Long) = {
    val timeMillis = (end - start) / 1000000
    val msgText = Doc.text(s"Compiled ${context.fileName} in").style(Style.Ansi.Fg.Blue)
    val timingText = Doc.text(s"${timeMillis}ms").style(Style.Ansi.Fg.White)
    val formattedMsg = Doc.hardLine + msgText & timingText
    formattedMsg.render(context.consoleWidth)
  }

  def compilationErrorTime(context: Printer.SourceContext, start: Long, end: Long) = {
    val timeMillis = (end - start) / 1000000
    val msgText = Doc.text(s"Compiling ${context.fileName} failed after").style(Style.Ansi.Fg.Red)
    val timingText = Doc.text(s"${timeMillis}ms").style(Style.Ansi.Fg.White)
    val formattedMsg = Doc.hardLine + msgText & timingText
    formattedMsg.render(context.consoleWidth)
  }

  def phaseTime(context: Printer.SourceContext, phaseName: String, start: Long, end: Long) = {
    val timeMillis = (end - start) / 1000000
    val msgText = Doc.text(s"Completed ${phaseName} for ${context.fileName} in").style(Style.Ansi.Fg.Blue)
    val timingText = Doc.text(s"${timeMillis}ms").style(Style.Ansi.Fg.White)
    val formattedMsg = Doc.hardLine + msgText & timingText
    formattedMsg.render(context.consoleWidth)
  }
}
