package inc.typechecker

import inc.common._

import cats.data.Chain
import cats.syntax.either._
import cats.syntax.functor._
import org.typelevel.paiges._
import java.lang.String
import scala.{ Boolean, Some, None, Either, Right, StringContext }
import scala.collection.immutable.List
import scala.Predef.ArrowAssoc
import com.typesafe.scalalogging.LazyLogging

class Gather(solve: Solve, context: Printer.SourceContext, isTraceEnabled: Boolean) extends LazyLogging {
  val kindchecker = new Kindchecker(context, isTraceEnabled: Boolean)

  def highlightSource(msg: String, pos: Pos): String =
    Printer.withSourceContext(context)(msg, pos, Style.Ansi.Fg.Yellow)

  def trace(name: String, pos: Pos, typ: Type) = {
    if (isTraceEnabled) {
      val formattedMsg = Doc.hardLine + Doc.text(name + ":") & Printer.print(typ)
      val formattedStr = formattedMsg.render(context.consoleWidth)
      logger.info(highlightSource(formattedStr, pos))
    }
  }

  def trace(name: String, pos: Pos, typ: TypeScheme) = {
    if (isTraceEnabled) {
      val formattedMsg = Doc.hardLine + Doc.text(name + ":") & Printer.print(typ)
      val formattedStr = formattedMsg.render(context.consoleWidth)
      logger.info(highlightSource(formattedStr, pos))
    }
  }

  def trace(name: String, pos: Pos, constraints: List[Constraint]) = {
    if (constraints.nonEmpty && isTraceEnabled) {
      val header = Doc.hardLine + Doc.text(name + ":") & (Doc.hardLine * 2)
      val formattedMsg = header + Doc.intercalate(Doc.hardLine, constraints.map(Printer.print))
      val formattedStr = formattedMsg.render(context.consoleWidth)
      logger.info(highlightSource(formattedStr, pos))
    }
  }

  def trace(name: String, constraints: List[Constraint]) = {
    if (constraints.nonEmpty && isTraceEnabled) {
      val header = Doc.hardLine + Doc.text(name + ":") & (Doc.hardLine * 2)
      val formattedMsg = header + Doc.intercalate(Doc.hardLine, constraints.map(Printer.print))
      val formattedStr = formattedMsg.render(context.consoleWidth)
      logger.info(formattedStr)
    }
  }

  def withTypeScheme(expr: Expr[Meta.Untyped], typ: TypeScheme): Infer[(Expr[Meta.Typed], List[Constraint])] = {
    val exprWithType = expr.map(_.withType(typ))
    Right((exprWithType, List.empty))
  }

  def withSimpleType(expr: Expr[Meta.Untyped], typ: Type): Infer[(Expr[Meta.Typed], List[Constraint])] =
    withTypeScheme(expr, TypeScheme(typ))

  def gather(
    expr: Expr[Meta.Untyped],
    env: Environment
  ): Infer[(Expr[Meta.Typed], List[Constraint])] =
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

      case ref @ Reference(_, name, meta)  =>
        env.declarations.get(ref.fullName).map { refMeta =>
          val refTyp = refMeta.typ
          val instTyp = refTyp.instantiate

          if (refTyp.bound.nonEmpty && isTraceEnabled) {
            val generalized = Printer.print(refTyp).render(context.consoleWidth)
            val instantiated = Printer.print(instTyp).render(context.consoleWidth)
            logger.info(NL + s"Instantiate ${generalized} as ${instantiated}")
          }

          trace(s"Reference to $name", meta.pos, instTyp)

          Right((expr.map(_.withSimpleType(instTyp)), List.empty))
        }.getOrElse(TypeError.singleton(meta.pos, s"Reference to undefined symbol: $name"))

      case If(cond, thenExpr, elseExpr, meta) =>
        for {
          // Gather constraints from the condition
          (c, condCst) <- gather(cond, env)

          Meta.Typed(_, condType, _) = c.meta

          _ = trace("If condition", c.meta.pos, condCst)

          // Gather constraints from the then expression
          (t, thenCst) <- gather(thenExpr, env)

          Meta.Typed(_, thenType, _) = t.meta

          _ = trace("Then expression", t.meta.pos, thenCst)

          // Gather constraints from the else expression
          (e, elseCst) <- gather(elseExpr, env)

          Meta.Typed(_, elseType, _) = e.meta

          _ = trace("Else expression", e.meta.pos, elseCst)

          // Emit a constraint that the condition must be Boolean
          condBoolean = List(Equal(condType.typ, Type.Boolean, meta.pos))

          // Emit a constraint that the then expression and the
          // else expression must have the same type
          thenElseEqual = List(Equal(thenType.typ, elseType.typ, meta.pos))

          expr = If(c, t, e, meta.withType(thenType))

          _ = trace("If expression", expr.meta.pos, condBoolean ++ thenElseEqual)

          constraints = condCst ++ thenCst ++ elseCst ++ condBoolean ++ thenElseEqual

        } yield (expr, constraints)

      case Lambda(params, body, meta) =>
        val typedParams = params.map {
          case Param(name, ascribedAs @ Some(typeScheme), meta) =>
            Param(name, ascribedAs, meta.withType(typeScheme))
          case Param(name, None, meta) =>
            Param(name, None, meta.withSimpleType(TypeVariable()))
        }

        val paramMappings = typedParams.map(p => p.name -> p.meta)

        typedParams.foreach {
          case Param(name, _, meta) =>
            trace(name, meta.pos, meta.typ)
        }

        for {
          // Gather constraints from the body with the params in scope
          (body, bodyCst) <- gather(body, env.withDeclarations(paramMappings))

          _ = trace("Lambda body", body.meta.pos, bodyCst)

          bodyTp = body.meta.typ.typ

          // Create a new function type
          funTp = Type.Function(typedParams.map(_.meta.typ.typ), bodyTp)

          expr = Lambda(typedParams, body, meta.withSimpleType(funTp))

          _ = trace("Lambda expression", expr.meta.pos, expr.meta.typ)

        } yield (expr, bodyCst)

      case Ascription(expr, ascribedAs, meta) =>
        for {
          (e, exprCst) <- gather(expr, env)

          _ = trace("Ascribed expression", e.meta.pos, exprCst)

          ascriptionCst =
            // Unless we know the ascription is correct
            if (e.meta.typ.typ == ascribedAs.typ)
              List.empty
            // Emit a constraint that the expression's type must match the ascription
            else
              List(Equal(e.meta.typ.typ, ascribedAs.typ, meta.pos))

          constraints = exprCst ++ ascriptionCst

          ascription = Ascription(e, ascribedAs, e.meta)

          _ = trace("Ascription", ascription.meta.pos, ascriptionCst)

        } yield (ascription, constraints)

      case Apply(fn, args, meta) =>
        val tv = TypeVariable()

        trace("Function application", meta.pos, tv)

        for {
          // Gather constraints from the function expression
          (f, fnCst) <- gather(fn, env)

          _ = trace("Function to apply", f.meta.pos, fnCst)

          initialResult = Either.right[List[TypeError], (Chain[Expr[Meta.Typed]], List[Constraint])]((Chain.empty, List.empty))

          // Gather constraints from the arg expressions
          (as, argCsts) <- args.zipWithIndex.foldLeft(initialResult) {
            case (resSoFar, (nextArg, idx)) =>
              for {
                (typedSoFar, cstsSoFar) <- resSoFar

                (typedArg, argCst) <- gather(nextArg, env)

                _ = trace(s"Argument ${idx + 1}", typedArg.meta.pos, argCst)

              } yield (typedSoFar :+ typedArg, cstsSoFar ++ argCst)
          }

          argsList = as.toList

          argTps = argsList.map(_.meta.typ.typ)

          // Create a new function type of the applied argument types and the inferred return type
          appliedTp = Type.Function(argTps.toList, tv)

          _ = trace("Applied type", meta.pos, appliedTp)

          fnTp = f.meta.typ.typ

          // Emit a constraint that the declared function type must match the way it has been applied
          declEqualsAppCst = List(Equal(fnTp, appliedTp, meta.pos))

          expr = Apply(f, argsList, meta.withType(TypeScheme(tv)))

        } yield (expr, fnCst ++ argCsts ++ declEqualsAppCst)
    }

  def gather(
    decl: TopLevelDeclaration[Meta.Untyped],
    env: Environment
  ): Infer[(TopLevelDeclaration[Meta.Typed], List[Constraint])] =
    decl match {
      case let @ Let(name, expr, meta) =>
        for {
          (checkedExpr, constraints) <- gather(expr, env)

          // We have to solve the constraints under a `let` before we generalize
          subst <- solve.solve(constraints)

          // Otherwise, the constraint substitution could not be applied to any bound type variables
          solvedExpr = checkedExpr.substitute(subst)
        } yield {
            val eTp = solvedExpr.meta.typ.typ
            val tp = TypeScheme.generalize(env, eTp)

            if (tp.bound.nonEmpty && isTraceEnabled) {
              val bound = tp.bound.map(t => Printer.print(t).render(context.consoleWidth))
              logger.info(NL + "Generalize: " + bound.mkString("[", ", ", "]"))
            }

            trace(name, meta.pos, tp)

            val checkedLet = let.copy(binding = checkedExpr, meta = meta.withType(tp))

            (checkedLet, constraints)
        }
      case data @ Data(name, tparams, cases, meta) =>
        val checkedCases: List[DataConstructor[Meta.Typed]] = cases.map {
          case constr @ DataConstructor(caseName, params, returnType, caseMeta) =>
            val checkedParams = params.map(_.withAscribedType)
            val typeScheme = TypeScheme(tparams, Type.Function(checkedParams.map(_.meta.typ.typ), returnType.typ))

            trace(caseName, caseMeta.pos, typeScheme)

            constr.copy(params = checkedParams, meta = caseMeta.withType(typeScheme))
        }

        val tyCon =
          TypeConstructor(name, KindVariable(), meta.pos)

        val typeScheme =
          if (tparams.isEmpty)
            TypeScheme(List.empty, tyCon)
          else
            TypeScheme(
              tparams,
              TypeApply(tyCon, tparams, KindVariable())
            )

        val checkedData = data.copy(
          cases = checkedCases,
          meta = meta.withType(typeScheme)
        )

        kindchecker.kindcheck(checkedData).map { d =>
          (d, List.empty)
        }
    }

  def gather(
    module: Module[Meta.Untyped],
    importedEnv: Environment
  ): Infer[(Module[Meta.Typed], List[Constraint])] =
    module match {
      case Module(_, _, _, decls, meta) =>

        val initialDecls = decls.flatMap {
          case Let(name, _, meta) =>
            List(name -> meta.withType(TypeScheme(List.empty, TypeVariable())))
          case Data(dataName, tparams, cases, _) =>
            cases.map {
              case DataConstructor(caseName, params, returnTyp, meta) =>
                val paramTypes = params.map { p => p.ascribedAs.get.typ }
                val dataTyp = TypeConstructor(dataName, returnTyp.typ.kind)
                val typeScheme = TypeScheme(tparams, Type.Function(paramTypes, dataTyp))
                caseName -> meta.withType(typeScheme)
            }
        }

        val initialTypes = decls.collect {
          case data @ Data(name, _, _, _) =>
            name -> data.kind
        }

        val initialEnv = importedEnv ++ Environment(initialDecls.toMap, initialTypes.toMap)

        val emptyRes: Infer[(Chain[TopLevelDeclaration[Meta.Typed]], Environment, List[Constraint])] =
          Right((Chain.empty, initialEnv, List.empty))

        val constraintsFromDecls = decls.foldLeft(emptyRes) {
          case (resSoFar, nextDecl) =>
            for {
              (checkedSoFar, envSoFar, constraintsSoFar) <- resSoFar
              (checkedDecl, constraints) <- gather(nextDecl, envSoFar)
              updatedEnv = envSoFar.withDeclaration(checkedDecl.name, checkedDecl.meta)
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
