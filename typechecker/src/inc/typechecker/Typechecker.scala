package inc.typechecker

import inc.common._
import java.lang.String
import scala.Boolean
import scala.collection.immutable.Map
import scala.Predef.{ ArrowAssoc, augmentString }
import com.typesafe.scalalogging.LazyLogging

object Typechecker extends Typechecker(false)

class Typechecker(isTraceEnabled: Boolean) extends LazyLogging {

  def trace(name: String, env: Environment) = {
    if (isTraceEnabled) {
      val formattedMsg = NL + name + ": " + (NL * 2) + env.map {
        case (nm, tp) =>
          val tpStr = Printer.print(tp).render(80)
          nm + ": " + tpStr
      }.mkString(NL)

      logger.info(formattedMsg)
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

      typeEnv = typedMod.declarations.map(decl => decl.name -> decl.meta.typ)

      _ = trace("Final type environment", typeEnv.toMap)

    } yield typedMod
  }
}
