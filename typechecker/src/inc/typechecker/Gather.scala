package inc.typechecker

import inc.common._

import cats.data.Chain
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.monoid._
import scala.{ Some, None, Right, StringContext }
import scala.collection.immutable.List
import scala.Predef.{ ???, ArrowAssoc }
import com.typesafe.scalalogging.LazyLogging

object Gather extends LazyLogging {

  def withTypeScheme(expr: Expr[Meta.Untyped], typ: TypeScheme): Infer[(Expr[Meta.Typed], List[TypeConstraint])] = {
    val exprWithType = expr.map(_.withType(typ))
    Right((exprWithType, List.empty))
  }

  def withSimpleType(expr: Expr[Meta.Untyped], typ: Type): Infer[(Expr[Meta.Typed], List[TypeConstraint])] =
    withTypeScheme(expr, TypeScheme(typ))

  def gather(
    pattern: Pattern[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): Infer[(Pattern[Meta.Typed], Environment[Meta.Typed], List[TypeConstraint])] = pattern match {
    case IdentPattern(name, meta) =>
      val tv = TypeVariable()
      Right((IdentPattern(name, meta.withSimpleType(tv)), env.withType(name, TypeScheme(tv)), List.empty))

    case ConstrPattern(name, alias, patterns, meta) =>
      val constrName @ ConstrName(_, _, _, _) = env.valueNames(name)
      val Type.Function(constrTpArgs) = env.types(constrName.shortName).instantiate
      val dataType = constrTpArgs.last
      val members = env.members(constrName)

      val initialResult = (Chain.empty[FieldPattern[Meta.Typed]], env, List.empty[TypeConstraint])

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

               val fieldTypeCst = List(EqualType(typedPat.meta.typ.typ, fieldType.get, meta.pos))

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
  ): Infer[(MatchCase[Meta.Typed], List[TypeConstraint])] = matchCase match {
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
  ): Infer[(Expr[Meta.Typed], List[TypeConstraint])] =
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
          condBoolean = List(EqualType(condType.typ, Type.Boolean, meta.pos))

          // Emit a constraint that the then expression and the
          // else expression must have the same type
          thenElseEqual = List(EqualType(thenType.typ, elseType.typ, meta.pos))

          expr = If(c, t, e, meta.withType(thenType))

          constraints = condCst ++ thenCst ++ elseCst ++ condBoolean ++ thenElseEqual

        } yield (expr, constraints)

      case Lambda(params, body, meta) =>
        val typedParams = params.map {
          case Param(name, None, meta) =>
            Param(name, None, meta.withSimpleType(TypeVariable()))
          case Param(name, ascribedAs @ Some(typeExpr), meta) =>
            val typeExprType = typeExpr.toType
            Param(
              name,
              ascribedAs.map(_.map(_.withSimpleType(typeExprType))),
              meta.withSimpleType(typeExprType))
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

          ascribedType = ascribedAs.toType

          ascriptionCst =
            // Unless we know the ascription is correct
            if (e.meta.typ.typ == ascribedType)
              List.empty
            // Emit a constraint that the expression's type must match the ascription
            else
              List(EqualType(e.meta.typ.typ, ascribedType, meta.pos))

          constraints = exprCst ++ ascriptionCst

          ascription = Ascription(e, ascribedAs.map(_.withSimpleType(ascribedType)), e.meta)

        } yield (ascription, constraints)

      case Apply(fn, args, meta) =>
        val tv = TypeVariable()

        for {
          // Gather constraints from the function expression
          (f, fnCst) <- gather(fn, env)

          initialResult = (Chain.empty[Expr[Meta.Typed]], List.empty[TypeConstraint])

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
          declEqualsAppCst = List(EqualType(fnTp, appliedTp, meta.pos))

          expr = Apply(f, argsList, meta.withType(TypeScheme(tv)))

        } yield (expr, fnCst ++ argCsts ++ declEqualsAppCst)

      case Match(matchExpr, cases, meta) =>
        val tv = TypeVariable()

        for  {
          (e, exprCsts) <- gather(matchExpr, env)

          initialResult = (Chain.empty[MatchCase[Meta.Typed]], List.empty[TypeConstraint])

          (cs, caseCsts) <- cases.zipWithIndex.foldM(initialResult) {
            case ((typedSoFar, cstsSoFar), (nextCase, _)) =>
              for {
                (typedCase, caseCst) <- gather(nextCase, env)
              } yield (typedSoFar :+ typedCase, cstsSoFar ++ caseCst)
          }

          matchExprCasesCst = cs.toList.map(c => EqualType(e.meta.typ.typ, c.pattern.meta.typ.typ, c.pattern.meta.pos))

          sameResultTypeCst = cs.toList.map(c => EqualType(tv, c.meta.typ.typ, c.meta.pos))

          checkedMatch = Match(e, cs.toList, meta.withType(TypeScheme(tv)))

        } yield (checkedMatch, exprCsts ++ caseCsts ++ matchExprCasesCst ++ sameResultTypeCst)
    }

  def gather(
    decl: TopLevelDeclaration[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): Infer[(TopLevelDeclaration[Meta.Typed], Environment[Meta.Typed], List[TypeConstraint])] =
    decl match {
      case let @ Let(_, expr, meta) =>
        for {
          (checkedExpr, constraints) <- gather(expr, env)

          // We have to solve the constraints under a `let` before we generalize
          subst <- Solve.solve(constraints)

          // Otherwise, the constraint substitution could not be applied to any bound type variables
          solvedExpr = checkedExpr.substitute(subst.subst)
        } yield {
            val eTp = solvedExpr.meta.typ.typ
            val tp = TypeScheme.generalize(env, eTp)
            val checkedLet = let.copy(binding = checkedExpr, meta = meta.withType(tp))
            (checkedLet, env, constraints)
        }
      case data @ Data(_, _, _, _) =>
        ???
    }

  def initialPass(
    module: Module[Meta.Untyped],
    decls: List[TopLevelDeclaration[Meta.Untyped]],
    importedEnv: Environment[Meta.Typed]
  ): Infer[Environment[Meta.Typed]] = {

    val symbolTable = module.symbolTable

    val initialEnv = importedEnv
      .withValueNames(symbolTable.valueNames)
      .withTypeNames(symbolTable.typeNames)

    decls.foldM(initialEnv) {

      case (env, Let(name, _, _)) =>
        env.withType(name, TypeScheme(List.empty, TypeVariable())).asRight

      case (env, data @ Data(_, tparams, cases, dataMeta)) =>
        val tyVars = tparams.map(_.toType.asInstanceOf[NamedTypeVariable])
        val tyConType = TypeScheme(tyVars, TypeApply(TypeConstructor(data.name, KindVariable()), tyVars, Atomic))

        val dataTypes = cases.map {
          case DataConstructor(caseName, params, _) =>
            val paramTypes = params.map(_.ascribedAs.get.toType)
            val typeScheme = TypeScheme(tyVars, Type.Function(paramTypes, tyConType.typ))
            caseName -> typeScheme
        }.toMap

        val dataMembers = for {
          DataConstructor(caseName, params, caseMeta) <- cases
          dataMember = dataMeta.name -> caseMeta.withType(dataTypes(caseName))
        } yield dataMember

        val constrMembers = for {
          DataConstructor(_, params, constrMeta) <- cases
          Param(_, ascribedAs, paramMeta) <- params
          fnType = TypeScheme(tyVars, Type.Function(List(tyConType.typ), ascribedAs.get.toType))
          constrMember = constrMeta.name -> paramMeta.withType(fnType)
        } yield constrMember

        val emptyConstrs = for {
          DataConstructor(name, params, constrMeta) <- cases
          if params.isEmpty
        } yield constrMeta.name -> List.empty[Meta.Typed]

        val allMembers = dataMembers ++ constrMembers

        val groupedMembers = allMembers.groupMap(_._1)(_._2) ++ emptyConstrs

        val updatedEnv = env
          .withTypes(dataTypes)
          .copy(members = env.members ++ groupedMembers)

        updatedEnv.asRight
    }
  }

  def gather(
    module: Module[Meta.Untyped],
    importedEnv: Environment[Meta.Typed]
  ): Infer[(Module[Meta.Typed], List[TypeConstraint])] = {
    initialPass(module, module.declarations, importedEnv).flatMap { initialEnv =>
      val emptyRes = (Chain.empty[TopLevelDeclaration[Meta.Typed]], initialEnv, List.empty[TypeConstraint])

      val constraintsFromDecls = module.declarations.foldM(emptyRes) {
        case ((checkedSoFar, envSoFar, constraintsSoFar), nextDecl) =>
          for {
            (checkedDecl, updatedEnv, constraints) <- gather(nextDecl, envSoFar)
            envWithDecl = updatedEnv.withType(checkedDecl.name, checkedDecl.meta.typ)
          } yield (checkedSoFar :+ checkedDecl, envWithDecl, constraintsSoFar ++ constraints)
      }

      for {
        (checked, checkedEnv, constraints) <- constraintsFromDecls

        dataDecls = checked.collect { case data @ Data(_, _, _, _) => data }
        dataRes = (initialEnv, Substitution.empty[KindVariable, Kind])

        (kindedEnv, kindSubst) <- dataDecls.foldM(dataRes) {
          case ((envSoFar, substSoFar), data) =>
            Kindchecker.kindcheck(data, envSoFar).map {
              case (env, subst) =>
                (envSoFar ++ env, substSoFar |+| subst)
            }
        }

        kindSubstWithDefault = kindSubst.withDefault(Atomic)

        constraintsWithKinds = constraints.map(cst => kindSubstWithDefault(cst))

        modWithKinds = kindSubstWithDefault(module.copy(
          declarations = checked.toList,
          meta = module.meta.withSimpleType(Type.Module)
        ))

      } yield (modWithKinds, constraintsWithKinds)
    }
  }
}
