package inc.common

import cats.data.Chain
import java.lang.String
import scala.{ Array, Int, Option, StringContext }
import scala.collection.immutable.{ List, Map, Nil, :: }
import scala.Predef.{ augmentString, genericArrayOps }

object Trace {
  def regionWithMargin(plainInputLines: Array[String], highlightedSource: fansi.Str, pos: Pos) = {
    val numberOfLines = plainInputLines.length

    def go(plainInputLines: List[(String, Int)], highlightedSource: fansi.Str, charIdx: Int, output: Chain[fansi.Str]): fansi.Str =
      plainInputLines match {
        case Nil =>
          fansi.Str.join(output.toList: _*)
        case (plainLine, lineIdx) :: plainLines =>
          val nextCharIdx = charIdx + 1 + plainLine.length

          val (highlightedLine, highLightedLines) =
            if (plainLine.length >= highlightedSource.length)
              (highlightedSource, fansi.Str(""))
            else
              highlightedSource.splitAt(plainLine.length + 1)

          if (pos.from > nextCharIdx || pos.to < charIdx)
            go(plainLines, highLightedLines, nextCharIdx, output)
          else {
            val lineNumber = lineIdx + 1
            val marginWidth = String.valueOf(numberOfLines).length + 1
            val margin = fansi.Color.White(fansi.Str(s"%${marginWidth}d".format(lineNumber) + '|'))
            go(plainLines, highLightedLines, nextCharIdx, output :+ margin :+ highlightedLine)
          }
      }

    go(plainInputLines.toList.zipWithIndex, highlightedSource, 0, Chain.empty[fansi.Str])
  }

  def withSourceContext(header: Option[String], msg: String, pos: Pos, colour: fansi.Attrs, source: String) = {
    if (pos.isEmpty)
      NL + msg
    else {
      val highlightedSource = fansi.Str(source, errorMode = fansi.ErrorMode.Sanitize).overlay(colour, pos.from, pos.to)
      val plainSourceLines = source.split('\n')
      val formattedLines = regionWithMargin(plainSourceLines, highlightedSource, pos)
      val formattedHeader = header.map(h => Blue(h + ":") + NL).getOrElse("")
      NL + formattedHeader + formattedLines + NL + msg
    }
  }

  def print(constraint: Constraint): String = constraint match {
    case Equal(l, r, _) =>
      print(l) + " \u2261 " + print(r)
  }

  def print(subst: Map[TypeVariable, Type]): String = {
    subst.map {
      case (tyVar, typ) =>
        print(tyVar) + White(" |-> ") + print(typ)
    }.mkString(", ")
  }

  def print(typ: Type): String = typ match {
    case TypeVariable(i) =>
      Yellow("T" + i)
    case TypeConstructor(nm, tyParams) =>
      if (tyParams.isEmpty)
        Yellow(nm)
      else if (nm == "->") {
        val args =
          if (tyParams.length == 2)
            print(tyParams.head)
          else
            tyParams.init.map(print).mkString(Yellow("("), ", ", Yellow(")"))

        args + Yellow(" -> ") + print(tyParams.last)

      } else {
        nm + tyParams.map(print).mkString("[", ", ", "]")
      }
  }

  def print(typ: TypeScheme): String = {
    if (typ.bound.isEmpty)
      print(typ.typ)
    else
      typ.bound.map(print).mkString("[", ", ", "]") + "{ " + print(typ.typ) + " }"
  }
}