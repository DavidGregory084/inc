package inc.typechecker

import inc.common._
import java.lang.String
import scala.{ Boolean, Right, Some }
import scala.collection.immutable.Map
import scala.Predef.{ ArrowAssoc, augmentString, ??? }
import cats.data.Chain
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

  def trace(name: String, env: Environment) = {
    lazy val formattedMsg = NL + name + ": " + (NL * 2) + env.map {
      case (nm, tp) =>
        nm + ": " + Printer.print(tp)
    }.mkString(NL)

    scribe.trace(formattedMsg)
  }

  def synth(
    expr: Expr[NameWithPos],
    env: Environment,
    source: String
  ): Infer[Expr[NamePosType]] = {
    ???
  }

  def check(
    decl: TopLevelDeclaration[NameWithPos],
    env: Environment,
    source: String
  ): Infer[TopLevelDeclaration[NamePosType]] = {
    ???
  }

  def synth(
    mod: Module[NameWithPos],
    env: Environment,
    source: String
  ): Infer[Module[NamePosType]] = {
    val emptyRes: Infer[(Chain[TopLevelDeclaration[NamePosType]], Environment)] =
      Right((Chain.empty, env))

    val checkedDecls = mod.declarations.foldLeft(emptyRes) {
      case (resSoFar, nextDecl) =>
        for {
          (checkedSoFar, envSoFar) <- resSoFar
          checkedDecl <- check(nextDecl, envSoFar, source)
          updatedEnv = envSoFar.updated(checkedDecl.name, checkedDecl.meta.typ)
        } yield (checkedSoFar :+ checkedDecl, updatedEnv)
    }

    checkedDecls.map {
      case (checked, _) =>
        mod.copy(
          declarations = checked.toList,
          meta = mod.meta.withSimpleType(Type.Module)
        )
    }

  }

  def typecheck(
    module: Module[NameWithPos],
    importedDecls: Map[String, TopLevelDeclaration[NameWithType]],
    source: String
  ): Infer[Module[NamePosType]] = {

    val initialEnv = importedDecls.mapValues(_.meta.typ)

    for {
      typedMod <- synth(module, initialEnv, source)

      typeEnv = typedMod.declarations.map(decl => decl.name -> decl.meta.typ)

      _ = trace("Final type environment", typeEnv.toMap)

    } yield typedMod
  }
}
