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

    // Ensure any unsolved kind variables are defaulted to *
    val defaultedMod = typedMod.defaultKinds
    val defaultedSubst = subst.defaultKinds

    // Apply the substition from the constraint solution to the module
    val solvedMod = defaultedSubst.apply(defaultedMod)

    val typeErrors = (gatherErrors ++ solveErrors).toList.distinct

    Either.cond(
      typeErrors.isEmpty,
      solvedMod,
      typeErrors
    )
  }
}
