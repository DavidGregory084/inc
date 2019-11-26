package inc.typechecker

import inc.common._
import java.lang.String
import scala.{ Boolean, Some }
import scala.collection.immutable.Map
import scala.Predef.{ ArrowAssoc, augmentString }
import scribe._
import scribe.format._

object Typechecker extends Typechecker(false)

class Typechecker(isTraceEnabled: Boolean) {
  if (isTraceEnabled) {
    this.logger.clearHandlers().withHandler(
      formatter = Formatter.simple,
      minimumLevel = Some(Level.Trace)
    ).replace()
  }

  def trace(name: String, env: Environment) = {
    lazy val formattedMsg = NL + name + ": " + (NL * 2) + env.map {
      case (nm, tp) =>
        nm + ": " + Printer.print(tp).render(80)
    }.mkString(NL)

    scribe.trace(formattedMsg)
  }

  def typecheck(
    module: Module[NameWithPos],
    importedDecls: Map[String, TopLevelDeclaration[NameWithType]],
    source: String
  ): Infer[Module[NamePosType]] = {
    val solve = new Solve(isTraceEnabled)
    val gather = new Gather(solve, isTraceEnabled)

    for {
      // Gather constraints from the module's definitions
      (mod, csts) <- gather.gather(module, importedDecls, source)

      // Try to solve the constraints
      subst       <- solve.solve(csts)

      _ = if (subst.nonEmpty) scribe.trace("Apply substitution: " + Printer.print(subst))

      // Apply the substitution from the constraint solution to the module
      typedMod = mod.substitute(subst)

      typeEnv = typedMod.declarations.map(decl => decl.name -> decl.meta.typ)

      _ = trace("Final type environment", typeEnv.toMap)

    } yield typedMod
  }
}
