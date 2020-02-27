package inc.typechecker

import inc.common._

import cats.data.Chain
import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import org.typelevel.paiges._
import java.lang.String
import scala.{ Boolean, Some, None, Right, StringContext }
import scala.collection.immutable.{ List, Map }
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

  def trace(context: Printer.SourceContext, name: String, env: Environment[Meta.Typed]) = {
    if (isTraceEnabled) {
      val header = Doc.hardLine + Doc.text(name + ":") + (Doc.hardLine * 2)

      val formattedTypesMsg = header + Doc.intercalate(Doc.hardLine, env.types.map {
        case (nm, meta) =>
          val tpStr = Printer.print(meta.typ)
          Doc.text(nm + ":") & tpStr
      })

      val formattedKindsMsg = Doc.intercalate(Doc.hardLine, env.kinds.map {
        case (nm, kind) =>
          val kindStr = Printer.print(kind)
          Doc.text(nm + ":") & kindStr
      })

      val formattedMsg = formattedTypesMsg + (Doc.hardLine * 2) + formattedKindsMsg

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
    pattern: Pattern[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): Infer[(Pattern[Meta.Typed], Environment[Meta.Typed], List[Constraint])] = pattern match {
    case IdentPattern(name, meta) =>
      val tv = TypeVariable()
      trace(s"Identifier pattern $name", meta.pos, tv)
      Right((IdentPattern(name, meta.withSimpleType(tv)), env.withType(name, TypeScheme(tv)), List.empty))

    case ConstrPattern(name, alias, patterns, meta) =>
      val constrName @ ConstrName(_, _, _, _) = env.names(name)
      val Type.Function(constrTpArgs) = env.types(constrName.shortName).instantiate
      val dataType = constrTpArgs.last
      val members = env.members(constrName)

      val initialResult = (Chain.empty[FieldPattern[Meta.Typed]], env, List.empty[Constraint])

      val typedPatterns = patterns.foldM(initialResult) {
        case ((typedSoFar, envSoFar, cstsSoFar), FieldPattern(field, None, fieldMeta)) =>
          val fieldType = for {
            member <- members.find(_.name == fieldMeta.name)
            Type.Function(memberTpArgs) = member.typ.instantiate
          } yield memberTpArgs.last

          val typedFieldPat = FieldPattern(field, None, fieldMeta.withSimpleType(fieldType.get))

          Right((typedSoFar :+ typedFieldPat, envSoFar.withType(field, TypeScheme(fieldType.get)), cstsSoFar))

        case ((typedSoFar, envSoFar, cstsSoFar), FieldPattern(field, Some(nextPat), fieldMeta)) =>
           gather(nextPat, envSoFar).map {
             case (typedPat, patEnv, patCst) =>
               val fieldType = for {
                 member <- members.find(_.name == fieldMeta.name)
                 Type.Function(memberTpArgs) = member.typ.instantiate
               } yield memberTpArgs.last

               val fieldTypeCst = List(Equal(typedPat.meta.typ.typ, fieldType.get, meta.pos))

               val typedFieldPat = FieldPattern(field, Some(typedPat), fieldMeta.withSimpleType(fieldType.get))

               (typedSoFar :+ typedFieldPat, envSoFar ++ patEnv, cstsSoFar ++ patCst ++ fieldTypeCst)
          }
      }

      typedPatterns.map {
        case (patterns, patEnv, patCsts) =>
          trace(s"Constructor pattern ${alias.getOrElse(name)}", meta.pos, dataType)
          val constrEnv = alias.map { a => patEnv.withType(a, TypeScheme(dataType)) }.getOrElse(patEnv)
          (ConstrPattern(name, alias, patterns.toList, meta.withSimpleType(dataType)), constrEnv, patCsts)
      }
  }

  def gather(
    matchCase: MatchCase[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): Infer[(MatchCase[Meta.Typed], List[Constraint])] = matchCase match {
    case MatchCase(pattern, resultExpr, meta) =>
      for {
        (p, patEnv, pCsts) <- gather(pattern, env)
        (r, rCsts) <- gather(resultExpr, patEnv)
        checked = MatchCase(p, r, meta.withType(r.meta.typ))
      } yield (checked, pCsts ++ rCsts)
  }

  def gather(
    expr: Expr[Meta.Untyped],
    env: Environment[Meta.Typed]
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
        env.types.get(ref.fullName).map { refTyp =>
          val instTyp = refTyp.instantiate

          if (refTyp.bound.nonEmpty && isTraceEnabled) {
            val generalized = Printer.print(refTyp).render(context.consoleWidth)
            val instantiated = Printer.print(instTyp).render(context.consoleWidth)
            logger.info(NL + s"Instantiate ${generalized} as ${instantiated}")
          }

          trace(s"Reference to $name", meta.pos, instTyp)

          Right((expr.map(_.withSimpleType(instTyp)), List.empty))
        }.getOrElse(TypeError.generic(meta.pos, s"Reference to undefined symbol: $name"))

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

        val paramMappings = typedParams.map(p => p.name -> p.meta.typ)

        typedParams.foreach {
          case Param(name, _, meta) =>
            trace(name, meta.pos, meta.typ)
        }

        for {
          // Gather constraints from the body with the params in scope
          (body, bodyCst) <- gather(body, env.withTypes(paramMappings))

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

          initialResult = (Chain.empty[Expr[Meta.Typed]], List.empty[Constraint])

          // Gather constraints from the arg expressions
          (as, argCsts) <- args.zipWithIndex.foldM(initialResult) {
            case ((typedSoFar, cstsSoFar), (nextArg, idx)) =>
              for {
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

      case Match(matchExpr, cases, meta) =>
        val tv = TypeVariable()

        trace("Match expression", meta.pos, tv)

        for  {
          (e, exprCsts) <- gather(matchExpr, env)

          _ = trace("Match target expression", meta.pos, exprCsts)

          initialResult = (Chain.empty[MatchCase[Meta.Typed]], List.empty[Constraint])

          (cs, caseCsts) <- cases.zipWithIndex.foldM(initialResult) {
            case ((typedSoFar, cstsSoFar), (nextCase, idx)) =>
              for {
                (typedCase, caseCst) <- gather(nextCase, env)

                _ = trace(s"Case ${idx + 1}", typedCase.meta.pos, caseCst)

              } yield (typedSoFar :+ typedCase, cstsSoFar ++ caseCst)
          }

          matchExprCasesCst = cs.toList.map(c => Equal(e.meta.typ.typ, c.pattern.meta.typ.typ, c.pattern.meta.pos))

          sameResultTypeCst = cs.toList.map(c => Equal(tv, c.meta.typ.typ, c.meta.pos))

          checkedMatch = Match(e, cs.toList, meta.withType(TypeScheme(tv)))

        } yield (checkedMatch, exprCsts ++ caseCsts ++ matchExprCasesCst ++ sameResultTypeCst)
    }

  def gather(
    decl: TopLevelDeclaration[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): Infer[(TopLevelDeclaration[Meta.Typed], Environment[Meta.Typed], List[Constraint])] =
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

            (checkedLet, env, constraints)
        }
      case data @ Data(_, _, _, _) =>
        (data.withAscribedTypes, env, List.empty).asRight
    }

  def initialPass(
    module: Module[Meta.Untyped],
    decls: List[TopLevelDeclaration[Meta.Untyped]],
    importedEnv: Environment[Meta.Typed]
  ): Infer[(Environment[Meta.Typed], Map[KindVariable, Kind])] = {

    val initialEnv = importedEnv.withNames(module.symbolTable.names)

    decls.foldM((initialEnv, Map.empty[KindVariable, Kind])) {

      case ((env, subst), Let(name, _, _)) =>
        (env.withType(name, TypeScheme(List.empty, TypeVariable())), subst).asRight

      case ((env, subst), data @ Data(_, tparams, cases, dataMeta)) =>
        val dataTypes = cases.map {
          case DataConstructor(caseName, params, returnTyp, _) =>
            val paramTypes = params.map(_.ascribedAs.get.typ)
            val typeScheme = TypeScheme(tparams, Type.Function(paramTypes, returnTyp.typ))
            caseName -> typeScheme
        }.toMap

        val dataMembers = for {
          DataConstructor(caseName, params, _, caseMeta) <- cases
          dataMember = dataMeta.name -> caseMeta.withType(dataTypes(caseName))
        } yield dataMember

        val constrMembers = for {
          DataConstructor(_, params, returnTyp, constrMeta) <- cases
          Param(_, ascribedAs, paramMeta) <- params
          fnType = TypeScheme(tparams, Type.Function(List(returnTyp.typ), ascribedAs.get.typ))
          constrMember = constrMeta.name -> paramMeta.withType(fnType)
        } yield constrMember

        val emptyConstrs = for {
          DataConstructor(name, params, _, constrMeta) <- cases
          if params.isEmpty
        } yield constrMeta.name -> List.empty[Meta.Typed]

        val allMembers = dataMembers ++ constrMembers

        val groupedMembers = allMembers.groupMap(_._1)(_._2) ++ emptyConstrs

        val updatedEnv = env
          .withTypes(dataTypes)
          .copy(members = env.members ++ groupedMembers)

        kindchecker.kindcheck(data.withAscribedTypes, updatedEnv).map {
          case (kindedEnv, updatedSubst) =>
            (kindedEnv, subst ++ updatedSubst)
        }

    }
  }

  def gather(
    module: Module[Meta.Untyped],
    importedEnv: Environment[Meta.Typed]
  ): Infer[(Module[Meta.Typed], List[Constraint])] = {
    initialPass(module, module.declarations, importedEnv).flatMap {
      case (initialEnv, kindSubst) =>
        trace(context, "Initial environment", initialEnv)

        val emptyRes = (Chain.empty[TopLevelDeclaration[Meta.Typed]], initialEnv, List.empty[Constraint])

        val constraintsFromDecls = module.declarations.foldM(emptyRes) {
          case ((checkedSoFar, envSoFar, constraintsSoFar), nextDecl) =>
            for {
              (checkedDecl, updatedEnv, constraints) <- gather(nextDecl, envSoFar)
              envWithDecl = updatedEnv.withType(checkedDecl.name, checkedDecl.meta.typ)
            } yield (checkedSoFar :+ checkedDecl, envWithDecl, constraintsSoFar ++ constraints)
        }

        constraintsFromDecls.map {
          case (checked, _, constraints) =>

            trace("Constraints", constraints)

            val constraintsWithKinds =
              constraints.map(_.substituteKinds(kindSubst).defaultKinds)

            val modWithKinds = module.copy(
              declarations = checked.toList,
              meta = module.meta.withSimpleType(Type.Module)
            ).substituteKinds(kindSubst).defaultKinds

            (modWithKinds, constraintsWithKinds)
        }
    }
  }
}
