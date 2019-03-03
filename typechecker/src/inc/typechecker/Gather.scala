package inc.typechecker

import inc.common._

import cats.data.Chain
import cats.syntax.either._
import cats.syntax.functor._
import java.lang.String
import scala.{ Boolean, Either, Right, Some, None, StringContext }
import scala.collection.immutable.{ List, Map }
import scala.Predef.{ ArrowAssoc, augmentString }
import scribe._
import scribe.format._

class Gather(isTraceEnabled: Boolean) {
  if (isTraceEnabled) {
    this.logger.withHandler(
      formatter = Formatter.simple,
      minimumLevel = Some(Level.Trace)
    ).replace()
  }

  def trace(name: String, pos: Pos, typ: Type, source: String) = {
    lazy val formattedMsg = name + ": " + Printer.print(typ)
    scribe.trace(Printer.withSourceContext(None, formattedMsg, pos, fansi.Color.Yellow, source))
  }

  def trace(name: String, pos: Pos, typ: TypeScheme, source: String) = {
    lazy val formattedMsg = name + ": " + Printer.print(typ)
    scribe.trace(Printer.withSourceContext(None, formattedMsg, pos, fansi.Color.Yellow, source))
  }

  def trace(name: String, pos: Pos, constraint: List[Constraint], source: String) = {
    lazy val formattedMsg = name + ": " + (NL * 2) +
      constraint.map(Printer.print).mkString(NL)
    scribe.trace(Printer.withSourceContext(None, formattedMsg, pos, fansi.Color.Yellow, source))
  }

  def trace(name: String, constraints: List[Constraint]) = {
    lazy val formattedMsg = name + ": " + (NL * 2) +
      constraints.map(Printer.print).mkString(NL)
    scribe.trace(formattedMsg)
  }

  def withTypeScheme(expr: Expr[NameWithPos], typ: TypeScheme): Infer[(Expr[NamePosType], List[Constraint])] = {
    val exprWithType = expr.map(_.withType(typ))
    Right((exprWithType, List.empty))
  }

  def withSimpleType(expr: Expr[NameWithPos], typ: Type): Infer[(Expr[NamePosType], List[Constraint])] =
    withTypeScheme(expr, TypeScheme(typ))

  def gather(
    expr: Expr[NameWithPos],
    env: Environment,
    source: String
  ): Infer[(Expr[NamePosType], List[Constraint])] =
    expr match {
      case int @ LiteralInt(_, _) =>
        withSimpleType(int, Type.Int)

      case long @ LiteralLong(_, _) =>
        withSimpleType(long, Type.Long)

      case float @ LiteralFloat(_, _) =>
        withSimpleType(float, Type.Float)

      case double @ LiteralDouble(_, _) =>
        withSimpleType(double, Type.Double)

      case bool @ LiteralBoolean(_, _) =>
        withSimpleType(bool, Type.Boolean)

      case char @ LiteralChar(_, _) =>
        withSimpleType(char, Type.Char)

      case str @ LiteralString(_, _) =>
        withSimpleType(str, Type.String)

      case unit @ LiteralUnit(_) =>
        withSimpleType(unit, Type.Unit)

      case Reference(name, meta)  =>
        env.get(name).map { typ =>
          trace(s"Reference to $name", meta.pos, typ, source)

          val (tp, cst) = typ.instantiate(meta.pos)

          if (cst.nonEmpty)
            scribe.trace(NL + s"Instantiate ${Printer.print(typ)}:${NL * 2}${cst.map(Printer.print).mkString(NL)}")

          Right((expr.map(_.withSimpleType(tp)), cst))
        }.getOrElse(TypeError.singleton(meta.pos, s"Reference to undefined symbol: $name"))

      case If(cond, thenExpr, elseExpr, meta) =>
        for {
          // Gather constraints from the condition
          (c, s1) <- gather(cond, env, source)

          NamePosType(_, _, condType) = c.meta

          _ = trace("If condition", c.meta.pos, s1, source)

          // Gather constraints from the then expression
          (t, s2) <- gather(thenExpr, env, source)

          NamePosType(_, _, thenType) = t.meta

          _ = trace("Then expression", t.meta.pos, s2, source)

          // Gather constraints from the else expression
          (e, s3) <- gather(elseExpr, env, source)

          NamePosType(_, _, elseType) = e.meta

          _ = trace("Else expression", e.meta.pos, s3, source)

          // Unify the condition with Boolean
          s4 = List(Equal(condType.typ, Type.Boolean, meta.pos))

          // Unify the then expression and the else expression
          s5 = List(Equal(thenType.typ, elseType.typ, meta.pos))

          expr = If(c, t, e, meta.withType(thenType))

          _ = trace("If expression", expr.meta.pos, s4 ++ s5, source)

        } yield (expr, s1 ++ s2 ++ s3 ++ s4 ++ s5)

      case Lambda(params, body, meta) =>
        val typedParams = params.map {
          case Param(name, meta) =>
            Param(name, meta.withSimpleType(TypeVariable()))
        }

        val paramMappings = typedParams.map(p => p.name -> p.meta.typ)

        typedParams.foreach {
          case Param(name, meta) =>
            trace(name, meta.pos, meta.typ, source)
        }

        for {
          // Gather constraints from the body with the params in scope
          (b, s) <- gather(body, env ++ paramMappings, source)

          _ = trace("Lambda body", b.meta.pos, b.meta.typ, source)

          bTp = b.meta.typ.typ

          // Create a new function type
          tp = Type.Function(typedParams.map(_.meta.typ.typ), bTp)

          // Apply the substitutions from the body
          expr = Lambda(typedParams, b, meta.withSimpleType(tp))

          _ = trace("Lambda expression", expr.meta.pos, expr.meta.typ, source)

        } yield (expr, s)

      case Apply(fn, args, meta) =>
        val tv = TypeVariable()

        trace("Function application", meta.pos, tv, source)

        for {
          // Gather constraints from the function expression
          (f, s1) <- gather(fn, env, source)

          _ = trace("Function to apply", f.meta.pos, f.meta.typ, source)

          initialResult = Either.right[List[TypeError], (Chain[Expr[NamePosType]], List[Constraint])]((Chain.empty, List.empty))

          // Gather constraints from the arg expressions
          (as, s2) <- args.zipWithIndex.foldLeft(initialResult) {
            case (resSoFar, (nextArg, idx)) =>
              for {
                (typedSoFar, cstsSoFar) <- resSoFar

                (typedArg, csts) <- gather(nextArg, env, source)

                _ = trace(s"Argument ${idx + 1}", typedArg.meta.pos, typedArg.meta.typ, source)

              } yield (typedSoFar :+ typedArg, cstsSoFar ++ csts)
          }

          argsList = as.toList

          argTps = argsList.map(_.meta.typ.typ)

          // Create a new function type
          appliedTp = Type.Function(argTps.toList, tv)

          _ = trace("Applied type", meta.pos, appliedTp, source)

          fnTp = f.meta.typ.typ

          // Unify the function type with the actual argument types
          s3 = List(Equal(fnTp, appliedTp, meta.pos))

          expr = Apply(f, argsList, meta.withType(TypeScheme(tv)))

        } yield (expr, s1 ++ s2 ++ s3)
    }

  def gather(
    decl: TopLevelDeclaration[NameWithPos],
    env: Environment,
    source: String
  ): Infer[(TopLevelDeclaration[NamePosType], List[Constraint])] =
    decl match {
      case let @ Let(name, expr, meta) =>
        gather(expr, env, source).flatMap {
          case (checkedExpr, constraints) =>
            val eTp = checkedExpr.meta.typ.typ
            val tp = TypeScheme.generalize(env, eTp)

            if (tp.bound.nonEmpty) scribe.trace(NL + "Generalize: " + tp.bound.map(Printer.print(_)).mkString("[", ", ", "]"))

            trace(name, meta.pos, tp, source)

            val checkedLet = let.copy(binding = checkedExpr, meta = meta.withType(tp))

            Right((checkedLet, constraints))
        }
    }

  def gather(
    module: Module[NameWithPos],
    importedDecls: Map[String, TopLevelDeclaration[NameWithType]],
    source: String
  ): Infer[(Module[NamePosType], List[Constraint])] =
    module match {
      case Module(_, _, _, decls, meta) =>

        val initialEnv = importedDecls.mapValues(_.meta.typ)

        val emptyRes: Infer[(Chain[TopLevelDeclaration[NamePosType]], Environment, List[Constraint])] =
          Right((Chain.empty, initialEnv, List.empty))

        val constraintsFromDecls = decls.foldLeft(emptyRes) {
          case (resSoFar, nextDecl) =>
            for {
              (checkedSoFar, envSoFar, constraintsSoFar) <- resSoFar
              (checkedDecl, constraints) <- gather(nextDecl, envSoFar, source)
              updatedEnv = envSoFar.updated(checkedDecl.name, checkedDecl.meta.typ)
            } yield (checkedSoFar :+ checkedDecl, updatedEnv, constraintsSoFar ++ constraints)
        }

        constraintsFromDecls.map {
          case (checked, _, constraints) =>

            trace("Constraints", constraints)

            val mod = module.copy(
              declarations = checked.toList,
              meta = meta.withSimpleType(Type.Module)
            )

            (mod, constraints)
        }
    }
}
