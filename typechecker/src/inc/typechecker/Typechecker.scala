package inc.typechecker

import inc.common._
import java.lang.String
import scala.{ Boolean, Some }
import scala.collection.immutable.Map
import scribe._
import scribe.format._

object Typechecker extends Typechecker(false)

class Typechecker(isTraceEnabled: Boolean) {
  if (isTraceEnabled) {
    this.logger.withHandler(
      formatter = Formatter.simple,
      minimumLevel = Some(Level.Trace)
    ).replace()
  }

  def typecheck(
    module: Module[NameWithPos],
    importedDecls: Map[String, TopLevelDeclaration[NameWithType]],
    source: String
  ): Infer[Module[NamePosType]] = {
    val gather = new Gather(isTraceEnabled)
    val solve = new Solve(isTraceEnabled)

    for {
      (mod, csts) <- gather.gather(module, importedDecls, source)
      subst       <- solve.solve(csts)
      _ = if (subst.nonEmpty) scribe.trace("Apply substitution: " + Printer.print(subst))
    } yield mod.substitute(subst)
  }
}
