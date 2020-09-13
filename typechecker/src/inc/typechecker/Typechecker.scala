package inc.typechecker

import inc.common._
import scala.Either

object Typechecker {
  def typecheck(
    module: Module[Meta.Untyped],
    importedEnv: Environment[Meta.Typed],
  ): Infer[Module[Meta.Typed]] = {
    // Gather type constraints from the module's definitions
    val (typedMod, Gather.State(env, gatherErrors, constraints)) =
      Gather.gather(module, importedEnv)

    // Try to solve the constraints
    val Solve.State(_, subst, solveErrors) =
      Solve.solve(env, constraints.toList)

    // Apply the substition from the constraint solution to the module
    val solvedMod = subst(typedMod)
    // Default any unsolved kind variables to kind *
      .defaultKinds

    val typeErrors = (gatherErrors ++ solveErrors).toList.distinct

    Either.cond(
      typeErrors.isEmpty,
      solvedMod,
      typeErrors
    )
  }
}
