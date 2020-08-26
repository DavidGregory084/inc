package inc.typechecker

import inc.common._
import com.typesafe.scalalogging.LazyLogging

object Typechecker extends LazyLogging {
  def typecheck(
    module: Module[Meta.Untyped],
    importedEnv: Environment[Meta.Typed],
  ): Infer[Module[Meta.Typed]] = {
    for {
      // Gather constraints from the module's definitions
      (mod, csts) <- Gather.gather(module, importedEnv)

      // Try to solve the constraints
      subst       <- Solve.solve(csts)

      // Apply the substitution from the constraint solution to the module
      typedMod = mod.substitute(subst)

    } yield typedMod
  }
}
