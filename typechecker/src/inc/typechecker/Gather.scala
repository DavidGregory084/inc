package inc.typechecker

import inc.common._

import cats.data.Chain
import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import scala.{ Some, None, Right, StringContext }
import scala.collection.immutable.{ List, Map }
import scala.Predef.ArrowAssoc
import com.typesafe.scalalogging.LazyLogging

object Gather extends LazyLogging {

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
          Right((expr.map(_.withSimpleType(instTyp)), List.empty))
        }.getOrElse(TypeError.generic(meta.pos, s"Reference to undefined symbol: $name"))

      case If(cond, thenExpr, elseExpr, meta) =>
        for {
          // Gather constraints from the condition
          (c, condCst) <- gather(cond, env)

          Meta.Typed(_, condType, _) = c.meta

          // Gather constraints from the then expression
          (t, thenCst) <- gather(thenExpr, env)

          Meta.Typed(_, thenType, _) = t.meta

          // Gather constraints from the else expression
          (e, elseCst) <- gather(elseExpr, env)

          Meta.Typed(_, elseType, _) = e.meta

          // Emit a constraint that the condition must be Boolean
          condBoolean = List(Equal(condType.typ, Type.Boolean, meta.pos))

          // Emit a constraint that the then expression and the
          // else expression must have the same type
          thenElseEqual = List(Equal(thenType.typ, elseType.typ, meta.pos))

          expr = If(c, t, e, meta.withType(thenType))

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

        for {
          // Gather constraints from the body with the params in scope
          (body, bodyCst) <- gather(body, env.withTypes(paramMappings))

          bodyTp = body.meta.typ.typ

          // Create a new function type
          funTp = Type.Function(typedParams.map(_.meta.typ.typ), bodyTp)

          expr = Lambda(typedParams, body, meta.withSimpleType(funTp))

        } yield (expr, bodyCst)

      case Ascription(expr, ascribedAs, meta) =>
        for {
          (e, exprCst) <- gather(expr, env)

          ascriptionCst =
            // Unless we know the ascription is correct
            if (e.meta.typ.typ == ascribedAs.typ)
              List.empty
            // Emit a constraint that the expression's type must match the ascription
            else
              List(Equal(e.meta.typ.typ, ascribedAs.typ, meta.pos))

          constraints = exprCst ++ ascriptionCst

          ascription = Ascription(e, ascribedAs, e.meta)

        } yield (ascription, constraints)

      case Apply(fn, args, meta) =>
        val tv = TypeVariable()

        for {
          // Gather constraints from the function expression
          (f, fnCst) <- gather(fn, env)

          initialResult = (Chain.empty[Expr[Meta.Typed]], List.empty[Constraint])

          // Gather constraints from the arg expressions
          (as, argCsts) <- args.zipWithIndex.foldM(initialResult) {
            case ((typedSoFar, cstsSoFar), (nextArg, _)) =>
              for {
                (typedArg, argCst) <- gather(nextArg, env)
              } yield (typedSoFar :+ typedArg, cstsSoFar ++ argCst)
          }

          argsList = as.toList

          argTps = argsList.map(_.meta.typ.typ)

          // Create a new function type of the applied argument types and the inferred return type
          appliedTp = Type.Function(argTps.toList, tv)

          fnTp = f.meta.typ.typ

          // Emit a constraint that the declared function type must match the way it has been applied
          declEqualsAppCst = List(Equal(fnTp, appliedTp, meta.pos))

          expr = Apply(f, argsList, meta.withType(TypeScheme(tv)))

        } yield (expr, fnCst ++ argCsts ++ declEqualsAppCst)

      case Match(matchExpr, cases, meta) =>
        val tv = TypeVariable()

        for  {
          (e, exprCsts) <- gather(matchExpr, env)

          initialResult = (Chain.empty[MatchCase[Meta.Typed]], List.empty[Constraint])

          (cs, caseCsts) <- cases.zipWithIndex.foldM(initialResult) {
            case ((typedSoFar, cstsSoFar), (nextCase, _)) =>
              for {
                (typedCase, caseCst) <- gather(nextCase, env)
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
      case let @ Let(_, expr, meta) =>
        for {
          (checkedExpr, constraints) <- gather(expr, env)

          // We have to solve the constraints under a `let` before we generalize
          subst <- Solve.solve(constraints)

          // Otherwise, the constraint substitution could not be applied to any bound type variables
          solvedExpr = checkedExpr.substitute(subst)
        } yield {
            val eTp = solvedExpr.meta.typ.typ
            val tp = TypeScheme.generalize(env, eTp)
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

        Kindchecker.kindcheck(data.withAscribedTypes, updatedEnv).map {
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
