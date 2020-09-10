package inc.typechecker

import inc.common._

import cats.data.Chain
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.monoid._
import cats.syntax.traverse._
import scala.{ Some, None, Right, StringContext }
import scala.collection.immutable.List
import scala.Predef.ArrowAssoc
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
    expr: TypeConstructorExpr[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): Infer[(TypeConstructorExpr[Meta.Typed], List[TypeConstraint])] = expr match {
    case TypeConstructorExpr(name, meta) =>
      env.types.get(name).map { typ =>
        Right((TypeConstructorExpr(name, meta.withType(typ)), List.empty))
      }.getOrElse(TypeError.generic(meta.pos, s"Reference to undefined type: ${name}"))
  }

  def gather(
    expr: TypeExpr[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): Infer[(TypeExpr[Meta.Typed], List[TypeConstraint])] = expr match {
    case TypeApplyExpr(typ, args, meta) =>
      val tpArgResult = (Chain.empty[TypeExpr[Meta.Typed]], List.empty[TypeConstraint])

      for {
        (tp, tpCst) <- gather(typ, env)

        (args, argCst) <- args.foldM(tpArgResult) {
          case ((typedSoFar, cstsSoFar), nextArg) =>
            for {
              (typedArg, argCst) <- gather(nextArg, env)
            } yield (typedSoFar :+ typedArg, cstsSoFar ++ argCst)
        }

        typedArgs = args.toList

        tAppType = TypeApply(tp.meta.typ.typ, typedArgs.map(_.meta.typ.typ), Atomic)

      } yield (TypeApplyExpr(tp, typedArgs, meta.withSimpleType(tAppType)), tpCst ++ argCst)

    case tyCon @ TypeConstructorExpr(_, _) =>
      gather(tyCon, env)
  }

  def gather(
    param: Param[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): Infer[(Param[Meta.Typed], List[TypeConstraint])] = param match {
    case param @ Param(_, None, meta) =>
      (param.copy(ascribedAs = None, meta = meta.withSimpleType(TypeVariable())), List.empty).asRight
    case Param(name, Some(typeExpr), meta) =>
      gather(typeExpr, env).map {
        case (tyExpr, tyExpCst) =>
          (param.copy(name, Some(tyExpr), meta.withSimpleType(tyExpr.meta.typ.typ)), tyExpCst)
      }
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

      case ref @ Reference(_, _, meta)  =>
        env.types.get(ref.fullName).map { refTyp =>
          pprint.pprintln(refTyp)
          val instTyp = refTyp.instantiate
          pprint.pprintln(instTyp)
          Right((expr.map(_.withSimpleType(instTyp)), List.empty))
        }.getOrElse(TypeError.generic(meta.pos, s"Reference to undefined value: ${ref.fullName}"))

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
        val paramResult = (Chain.empty[Param[Meta.Typed]], List.empty[TypeConstraint])

        for {
          (params, paramCst) <- params.foldM(paramResult) {
            case ((paramsSoFar, cstsSoFar), param) =>
              gather(param, env).map {
                case (param, paramCst) =>
                  (paramsSoFar :+ param, cstsSoFar ++ paramCst)
              }
          }

          typedParams = params.toList

          paramMappings = typedParams.map(p => p.name -> p.meta.typ)

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

          (a, ascCst) <- gather(ascribedAs, env)

          ascribedType = a.meta.typ.typ

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
    constr: DataConstructor[Meta.Untyped],
    dataType: TypeScheme,
    env: Environment[Meta.Typed]
  ): Infer[(DataConstructor[Meta.Typed], List[TypeConstraint])] = constr match {
    case DataConstructor(_, params, meta) =>
      val paramResult = (Chain.empty[Param[Meta.Typed]], List.empty[TypeConstraint])
      val envMemberTypes = env.members(meta.name)
        .map(meta => meta.name -> meta.typ.typ)
        .toMap

      for {
        (typedParams, paramsCst) <- params.foldM(paramResult) {
          case ((typedSoFar, cstsSoFar), nextParam) =>
            for {
              (typedParam, paramCst) <- gather(nextParam, env)
              typedParamPos = typedParam.meta.pos
              typedParamName = typedParam.meta.name
              typedParamType = typedParam.meta.typ.typ
              envEqualsAscribedCst = List(EqualType(typedParamType, envMemberTypes(typedParamName), meta.pos))
            } yield (typedSoFar :+ typedParam, cstsSoFar ++ paramCstk
        }

        typedParamTypes = typedParams.toList.map(_.meta.typ.typ)

        tyScheme = dataType.copy(typ = Type.Function(typedParamTypes, dataType.typ))

        updatedConstr = constr.copy(
          params = typedParams.toList,
          meta = meta.withType(tyScheme)
        )

      } yield (updatedConstr, paramsCst)
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
      case data @ Data(dataName, typeParams, cases, _) =>
        val casesResult = (Chain.empty[DataConstructor[Meta.Typed]], List.empty[TypeConstraint])
        val dataType = env.types(dataName)
        val typeParamTypes = dataType.bound
        val envWithTparams = env.withTypes(typeParamTypes.map(tparam => tparam.name -> TypeScheme(tparam)))

        for {
          typedTparams <- typeParams.zip(typeParamTypes).traverse {
            case (param, paramType) =>
              param.map(_.withSimpleType(paramType)).asRight
          }

          (cases, casesCst) <- cases.foldM(casesResult) {
            case ((casesSoFar, cstsSoFar), cse) =>
              gather(cse, dataType, envWithTparams).map {
                case (cse, cseCst) =>
                  (casesSoFar :+ cse, cstsSoFar ++ cseCst)
              }
          }

          updatedData = data.copy(
            typeParams = typedTparams,
            cases = cases.toList,
            meta = data.meta.withType(dataType)
          )

        } yield (updatedData, env, casesCst)
    }

  def initialPass(
    module: Module[Meta.Untyped],
    decls: List[TopLevelDeclaration[Meta.Untyped]],
    importedEnv: Environment[Meta.Typed]
  ): Infer[(Environment[Meta.Typed], List[TypeConstraint])] = {

    val symbolTable = module.symbolTable

    val initialEnv = importedEnv
      .withValueNames(symbolTable.valueNames)
      .withTypeNames(symbolTable.typeNames)

    decls.foldM(initialEnv) {

      case (env, Let(name, _, _)) =>
        env.withType(name, TypeScheme(List.empty, TypeVariable())).asRight

      case (env, data @ Data(dataName, tparams, cases, dataMeta)) =>
        val tyVars = tparams.map(tp => TypeVariable.named(tp.name))
        val dataConType = TypeConstructor(data.name, KindVariable())
        val dataAppType = TypeScheme(tyVars, TypeApply(dataConType, tyVars, Atomic))

        val constrTypes = cases.map {
          case DataConstructor(caseName, params, _) =>
            val paramTypes = params.map(_ => TypeVariable())
            val typeScheme = TypeScheme(tyVars, Type.Function(paramTypes, dataAppType.typ))
            caseName -> typeScheme
        }.toMap

        val dataMembers = for {
          DataConstructor(caseName, params, caseMeta) <- cases
          dataMember = dataMeta.name -> caseMeta.withType(constrTypes(caseName))
        } yield dataMember

        val constrMembers = for {
          DataConstructor(_, params, constrMeta) <- cases
          Param(_, _, paramMeta) <- params
          fnType = TypeScheme(tyVars, Type.Function(List(dataAppType.typ), TypeVariable()))
          constrMember = constrMeta.name -> paramMeta.withType(fnType)
        } yield constrMember

        val emptyConstrs = for {
          DataConstructor(name, params, constrMeta) <- cases
          if params.isEmpty
        } yield constrMeta.name -> List.empty[Meta.Typed]

        val allMembers = dataMembers ++ constrMembers

        val groupedMembers = allMembers.groupMap(_._1)(_._2) ++ emptyConstrs

        val updatedEnv = env
          .withType(dataName, dataAppType)
          .withTypes(constrTypes)
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

        constraintsWithKinds = constraints.map(cst => kindSubst(cst))

        modWithKinds = kindSubst(module.copy(
          declarations = checked.toList,
          meta = module.meta.withSimpleType(Type.Module)
        )).defaultKinds

      } yield (modWithKinds, constraintsWithKinds)
    }
  }
}
