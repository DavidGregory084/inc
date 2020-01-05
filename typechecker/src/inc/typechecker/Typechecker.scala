package inc.typechecker

import inc.common._
import java.lang.String
import org.typelevel.paiges._
import scala.Boolean
import scala.collection.immutable.List
import com.typesafe.scalalogging.LazyLogging

object Typechecker extends Typechecker(false)

class Typechecker(isTraceEnabled: Boolean) extends LazyLogging {

  def trace(context: Printer.SourceContext, name: String, env: Environment) = {
    if (isTraceEnabled) {
      val header = Doc.hardLine + Doc.text(name + ":") + (Doc.hardLine * 2)

      val formattedMsg = header + Doc.intercalate(Doc.hardLine, env.declarations.map {
        case (nm, meta) =>
          val tpStr = Printer.print(meta.typ)
          Doc.text(nm + ":") & tpStr
      })

      val formattedStr = formattedMsg.render(context.consoleWidth)

      logger.info(formattedStr)
    }
  }

  def trace(context: Printer.SourceContext, msg: String, declarations: List[TopLevelDeclaration[Meta.Typed]]) = {
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
    module: Module[Meta.Untyped],
    importedEnv: Environment,
    context: Printer.SourceContext
  ): Infer[Module[Meta.Typed]] = {
    val solve = new Solve(context, isTraceEnabled)
    val gather = new Gather(solve, context, isTraceEnabled)

    for {
      // Gather constraints from the module's definitions
      (mod, csts) <- gather.gather(module, importedEnv)

      // Try to solve the constraints
      subst       <- solve.solve(csts)

      _ = if (subst.nonEmpty && isTraceEnabled) {
        val substitution = Printer.print(subst).render(context.consoleWidth)
        logger.info(NL + "Apply substitution: " + substitution)
      }

      // Apply the substitution from the constraint solution to the module
      typedMod = mod.substitute(subst)

      _ = trace(context, "Final type environment", typedMod.environment)

    } yield typedMod
  }
}
