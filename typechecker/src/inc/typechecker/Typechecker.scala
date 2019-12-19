package inc.typechecker

import inc.common._
import java.lang.String
import org.typelevel.paiges._
import scala.Boolean
import scala.collection.immutable.{ List, Map }
import com.typesafe.scalalogging.LazyLogging

object Typechecker extends Typechecker(false)

class Typechecker(isTraceEnabled: Boolean) extends LazyLogging {

  def trace(context: Printer.SourceContext, name: String, env: Environment) = {
    if (isTraceEnabled) {
      val header = Doc.hardLine + Doc.text(name + ":") + (Doc.hardLine * 2)

      val formattedMsg = header + Doc.intercalate(Doc.hardLine, env.map {
        case (nm, tp) =>
          val tpStr = Printer.print(tp)
          Doc.text(nm + ":") & tpStr
      })

      val formattedStr = formattedMsg.render(context.consoleWidth)

      logger.info(formattedStr)
    }
  }

  def trace(context: Printer.SourceContext, msg: String, declarations: List[TopLevelDeclaration[NamePosType]]) = {
    if (isTraceEnabled) {
      val header = Doc.hardLine + Doc.text(msg + ":") + (Doc.hardLine * 2)

      val formattedMsg = header + Doc.intercalate(Doc.hardLine, declarations.flatMap {
        case let @ Let(_, _, _) =>
          val tpStr = Printer.print(let.meta.typ)
          List(Doc.text(let.name + ":") & tpStr)
        case data @ Data(_, _, _, _) =>
          data.cases.map {
            case constr @ DataConstructor(_, _, _, _) =>
              val tpStr = Printer.print(constr.meta.typ)
              Doc.text(constr.name + ":") & tpStr
          }
      })

      val formattedStr = formattedMsg.render(context.consoleWidth)

      logger.info(formattedStr)
    }
  }

  def typecheck(
    module: Module[NameWithPos],
    importedDecls: Map[String, TopLevelDeclaration[NameWithType]],
    context: Printer.SourceContext
  ): Infer[Module[NamePosType]] = {
    val solve = new Solve(context, isTraceEnabled)
    val gather = new Gather(solve, context, isTraceEnabled)

    for {
      // Gather constraints from the module's definitions
      (mod, csts) <- gather.gather(module, importedDecls)

      // Try to solve the constraints
      subst       <- solve.solve(csts)

      _ = if (subst.nonEmpty && isTraceEnabled) {
        val substitution = Printer.print(subst).render(context.consoleWidth)
        logger.info(NL + "Apply substitution: " + substitution)
      }

      // Apply the substitution from the constraint solution to the module
      typedMod = mod.substitute(subst)

      _ = trace(context, "Final type environment", typedMod.declarations)

    } yield typedMod
  }
}
