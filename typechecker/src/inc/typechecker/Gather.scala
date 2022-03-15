package inc.typechecker

import cats.Monoid
import cats.data.Chain
import cats.syntax.foldable._
import cats.syntax.monoid._
import inc.common._

import scala.None
import scala.Predef.ArrowAssoc
import scala.Some
import scala.StringContext
import scala.collection.immutable.List

object Gather {
  case class State(
    env: Environment[Meta.Typed],
    errors: Chain[TypeError],
    constraints: List[TypeConstraint]
  ) {
    def withEnv(newEnv: Environment[Meta.Typed]) =
      copy(env = newEnv)
    def withError(newError: TypeError) =
      copy(errors = errors :+ newError)
    def withErrors(newErrors: Chain[TypeError]) =
      copy(errors = errors ++ newErrors)
    def withConstraint(newConstraint: TypeConstraint) =
      copy(constraints = constraints ++ List(newConstraint))
    def withConstraints(newConstraints: List[TypeConstraint]) =
      copy(constraints = constraints ++ newConstraints)
    def substituteTypes(subst: Substitution[TypeVariable, Type]): State =
      copy(
        env = env.substituteTypes(subst.subst),
        constraints = constraints.map(_.substitute(subst.subst))
      )
    def substituteKinds(subst: Substitution[KindVariable, Kind]): State =
      copy(
        env = env.substituteKinds(subst.subst),
        constraints = constraints.map(_.substituteKinds(subst.subst))
      )
    def defaultKinds: State =
      copy(env = env.defaultKinds, constraints = constraints.map(_.defaultKinds))
  }
  object State {
    val empty                              = State(Environment.empty, Chain.empty, List.empty)
    def init(env: Environment[Meta.Typed]) = State(env, Chain.empty, List.empty)
    implicit def monoidForGatherState: Monoid[State] = new Monoid[State] {
      val empty: State =
        State.empty
      def combine(l: State, r: State): State =
        if (l.eq(State.empty))
          r
        else if (r.eq(State.empty))
          l
        else
          State(l.env ++ r.env, l.errors ++ r.errors, l.constraints ++ r.constraints)
    }
  }

  def gather(
    typExpr: TypeConstructorExpr[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): (TypeConstructorExpr[Meta.Typed], State) = typExpr match {
    case TypeConstructorExpr(name, meta) =>
      env.types
        .get(name)
        .map { envTyp =>
          val instTyp        = envTyp.instantiate
          val checkedTypExpr = TypeConstructorExpr(name, meta.withSimpleType(instTyp))
          (checkedTypExpr, State.empty)
        }
        .getOrElse {
          // Name resolution should prevent us from getting here
          val checkedTypExpr = TypeConstructorExpr(name, meta.withSimpleType(ErrorType))
          val typeError      = TypeError.generic(meta.pos, s"Reference to undefined type: ${name}")
          (checkedTypExpr, State.empty.withError(typeError))
        }
  }

  def gather(
    typExpr: TypeExpr[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): (TypeExpr[Meta.Typed], State) = typExpr match {

    case tyCon @ TypeConstructorExpr(_, _) =>
      gather(tyCon, env)

    case TypeApplyExpr(tyCon @ TypeConstructorExpr("->", _), tpArgs, meta) =>
      val (checkedArgs, argsState) = tpArgs.foldMap { nextArg =>
        val (arg, argState) = gather(nextArg, env)
        (Chain.one(arg), argState)
      }

      val tyArgTypes = checkedArgs.map(_.meta.typ.typ).toList
      val tyAppType  = Type.Function(tyArgTypes.init, tyArgTypes.last)

      val checkedTypeExpr = TypeApplyExpr(
        typ = tyCon.copy(meta = tyCon.meta.withSimpleType(tyAppType.typ)),
        args = checkedArgs.toList,
        meta = meta.withSimpleType(tyAppType)
      )

      (checkedTypeExpr, argsState)

    case TypeApplyExpr(typ, args, meta) =>
      val (checkedTyp, typState) = gather(typ, env)

      val (checkedArgs, argsState) = args.foldMap { nextArg =>
        val (arg, argState) = gather(nextArg, env)
        (Chain.one(arg), argState)
      }

      val tyArgTypes = checkedArgs.map(_.meta.typ.typ)
      val tyAppType  = TypeApply(checkedTyp.meta.typ.typ, tyArgTypes.toList, Atomic)

      val checkedTypeExpr = TypeApplyExpr(
        typ = checkedTyp,
        args = checkedArgs.toList,
        meta = meta.withSimpleType(tyAppType)
      )

      (checkedTypeExpr, typState |+| argsState)
  }

  def gather(
    param: Param[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): (Param[Meta.Typed], State) = param match {
    case Param(_, Some(ascribedAs), meta) =>
      val (asc, ascState) = gather(ascribedAs, env)

      val checkedParam = param.copy(ascribedAs = Some(asc), meta = meta.withType(asc.meta.typ))

      (checkedParam, ascState)

    case Param(_, None, meta) =>
      val checkedParam = param.copy(ascribedAs = None, meta = meta.withSimpleType(TypeVariable()))

      (checkedParam, State.empty)
  }

  def gather(
    pattern: Pattern[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): (Pattern[Meta.Typed], Environment[Meta.Typed], State) = pattern match {
    case IdentPattern(name, meta) =>
      val tv = TypeScheme(TypeVariable())

      val checkedPattern = IdentPattern(
        name,
        meta.withType(tv)
      )

      val updatedEnv = env.withType(name, tv)

      (checkedPattern, updatedEnv, State.empty)

    case ConstrPattern(name, alias, patterns, meta) =>
      val constrName @ ConstrName(_, _, _, _) = env.valueNames(name)
      val members                             = env.members(constrName)
      val envConstrType                       = env.types(name)

      // We need to keep this substitution
      val instantiationSubst =
        envConstrType.instantiateSubst

      // We want to use it to instantiate our constructor type
      val Type.Function(constrTpArgs) =
        envConstrType.typ.substitute(instantiationSubst.subst)

      val dataType = constrTpArgs.last

      val (checkedPatterns, patsEnv, patsState) = patterns.foldMap {
        case FieldPattern(fieldName, None, fieldMeta) =>
          val fieldType = for {
            member <- members.find(_.name == fieldMeta.name)
            // And we need to use the same substitution for our patterns too
            Type.Function(memberTpArgs) = member.typ.typ.substitute(instantiationSubst.subst)
          } yield memberTpArgs.last

          val checkedFieldPat = Chain.one(
            FieldPattern(
              name = fieldName,
              pattern = None,
              meta = fieldMeta.withSimpleType(fieldType.get)
            )
          )

          val updatedEnv = env.withType(fieldName, TypeScheme(fieldType.get))

          (checkedFieldPat, updatedEnv, State.empty)

        case FieldPattern(fieldName, Some(innerPat), fieldMeta) =>
          val fieldType = for {
            member <- members.find(_.name == fieldMeta.name)
            Type.Function(memberTpArgs) = member.typ.typ.substitute(instantiationSubst.subst)
          } yield memberTpArgs.last

          val (checkedInnerPat, innerPatEnv, innerPatState) = gather(innerPat, env)

          val patType    = checkedInnerPat.meta.typ.typ
          val patTypeCst = EqualType(patType, fieldType.get, fieldMeta.pos)

          val checkedFieldPat = Chain.one(
            FieldPattern(
              name = fieldName,
              pattern = Some(checkedInnerPat),
              meta = meta.withSimpleType(fieldType.get)
            )
          )

          val newState = innerPatState.withConstraint(patTypeCst)

          (checkedFieldPat, innerPatEnv, newState)
      }

      val updatedEnv = env ++ alias
        .map { a =>
          patsEnv.withType(a, TypeScheme(dataType))
        }
        .getOrElse {
          patsEnv
        }

      val checkedConstrPat = ConstrPattern(
        name = name,
        alias = alias,
        patterns = checkedPatterns.toList,
        meta = meta.withSimpleType(dataType)
      )

      (checkedConstrPat, updatedEnv, patsState)
  }

  def gather(
    matchCase: MatchCase[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): (MatchCase[Meta.Typed], State) = matchCase match {
    case MatchCase(pattern, resultExpr, meta) =>
      val (checkedPat, patEnv, patState) = gather(pattern, env)
      val (checkedExpr, exprState)       = gather(resultExpr, patEnv)

      val checkedCase = MatchCase(
        checkedPat,
        checkedExpr,
        meta.withType(checkedExpr.meta.typ)
      )

      (checkedCase, patState |+| exprState)
  }

  def gather(
    expr: Expr[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): (Expr[Meta.Typed], State) = expr match {
    case int @ LiteralInt(_, _) =>
      (int.copy(meta = int.meta.withSimpleType(Type.Int)), State.empty)

    case long @ LiteralLong(_, _) =>
      (long.copy(meta = long.meta.withSimpleType(Type.Long)), State.empty)

    case float @ LiteralFloat(_, _) =>
      (float.copy(meta = float.meta.withSimpleType(Type.Float)), State.empty)

    case double @ LiteralDouble(_, _) =>
      (double.copy(meta = double.meta.withSimpleType(Type.Double)), State.empty)

    case boolean @ LiteralBoolean(_, _) =>
      (boolean.copy(meta = boolean.meta.withSimpleType(Type.Boolean)), State.empty)

    case char @ LiteralChar(_, _) =>
      (char.copy(meta = char.meta.withSimpleType(Type.Char)), State.empty)

    case string @ LiteralString(_, _) =>
      (string.copy(meta = string.meta.withSimpleType(Type.String)), State.empty)

    case unit @ LiteralUnit(_) =>
      (unit.copy(meta = unit.meta.withSimpleType(Type.Unit)), State.empty)

    case ref @ Reference(_, _, meta) =>
      env.types
        .get(ref.fullName)
        .map { refTyp =>
          val instTyp    = refTyp.instantiate
          val checkedRef = ref.copy(meta = meta.withSimpleType(instTyp))
          (checkedRef, State.empty)
        }
        .getOrElse {
          // Name resolution should prevent us from getting here
          val checkedRef = ref.copy(meta = meta.withSimpleType(ErrorType))
          val typeError =
            TypeError.generic(meta.pos, s"Reference to undefined symbol: '${ref.fullName}'")
          (checkedRef, State.empty.withError(typeError))
        }

    case Ascription(expr, ascribedAs, meta) =>
      val (checkedExpr, exprState) = gather(expr, env)

      val exprType = checkedExpr.meta.typ.typ

      val (checkedTyExp, tyExpState) = gather(ascribedAs, env)

      val ascribedType = checkedTyExp.meta.typ.typ

      val ascriptionCst =
        // Unless we know the ascription is correct
        if (exprType == ascribedType)
          List.empty
        // Emit a constraint that the expression's type must match the ascription
        else
          List(EqualType(exprType, ascribedType, meta.pos))

      val checkedAsc = Ascription(
        checkedExpr,
        checkedTyExp,
        meta.withSimpleType(ascribedType)
      )

      val newState = (exprState |+| tyExpState).withConstraints(ascriptionCst)

      (checkedAsc, newState)

    case If(cond, thenExpr, elseExpr, meta) =>
      val (c, cState)                      = gather(cond, env)
      val Meta.Typed(_, condType, condPos) = c.meta

      val (t, tState)                = gather(thenExpr, env)
      val Meta.Typed(_, thenType, _) = t.meta

      val (e, eState)                = gather(elseExpr, env)
      val Meta.Typed(_, elseType, _) = e.meta

      val condBoolean   = EqualType(condType.typ, Type.Boolean, condPos)
      val thenElseEqual = EqualType(thenType.typ, elseType.typ, meta.pos)

      val checkedExpr = If(c, t, e, meta.withType(thenType))
      val constraints = List(condBoolean, thenElseEqual)

      val newState = (cState |+| tState |+| eState).withConstraints(constraints)

      (checkedExpr, newState)

    case Lambda(params, body, meta) =>
      val (checkedParams, paramsState) = params.foldMap { nextParam =>
        val (checkedParam, paramState) = gather(nextParam, env)
        (Chain.one(checkedParam), paramState)
      }

      val paramMappings = checkedParams.toList.map(p => p.name -> p.meta.typ)
      val paramTypes    = checkedParams.toList.map(_.meta.typ.typ)

      val (checkedBody, bodyState)   = gather(body, env.withTypes(paramMappings))
      val Meta.Typed(_, bodyType, _) = checkedBody.meta

      val funType = Type.Function(paramTypes, bodyType.typ)

      val expr = Lambda(checkedParams.toList, checkedBody, meta.withSimpleType(funType))

      val newState = (paramsState |+| bodyState)

      (expr, newState)

    case Apply(fn, args, meta) =>
      val tv = TypeVariable()

      val (checkedFn, fnState) = gather(fn, env)

      val (checkedArgs, argsState) = args.foldMap { nextArg =>
        val (arg, argState) = gather(nextArg, env)
        (Chain.one(arg), argState)
      }

      val declaredType = checkedFn.meta.typ.typ
      val argTypes     = checkedArgs.toList.map(_.meta.typ.typ)
      val appliedType  = Type.Function(argTypes, tv)

      val declEqualsAppCst = EqualType(declaredType, appliedType, meta.pos)

      val checkedExpr = Apply(
        checkedFn,
        checkedArgs.toList,
        meta.withSimpleType(tv)
      )

      val newState = (fnState |+| argsState).withConstraint(declEqualsAppCst)

      (checkedExpr, newState)

    case Match(matchExpr, cases, meta) =>
      val tv = TypeVariable()

      val (checkedExpr, exprState) = gather(matchExpr, env)

      val (checkedCases, casesState) = cases.foldMap { nextCase =>
        val (cse, caseState) = gather(nextCase, env)
        (Chain.one(cse), caseState)
      }

      val casesCsts = checkedCases.toList.flatMap { cse =>
        List(
          // All cases should have the same result
          EqualType(tv, cse.meta.typ.typ, cse.meta.pos),
          // All case patterns should have the same type as the matched expr
          EqualType(checkedExpr.meta.typ.typ, cse.pattern.meta.typ.typ, cse.meta.pos)
        )
      }

      val checkedMatch = Match(
        checkedExpr,
        checkedCases.toList,
        meta.withSimpleType(tv)
      )

      val newState = (exprState |+| casesState)
        .withConstraints(casesCsts)

      (checkedMatch, newState)
  }

  def gather(
    constr: DataConstructor[Meta.Untyped],
    dataType: TypeScheme,
    env: Environment[Meta.Typed]
  ): (Chain[DataConstructor[Meta.Typed]], State) = constr match {
    case constr @ DataConstructor(_, params, meta) =>
      val envMemberTypes = env
        .members(constr.meta.name)
        .map { meta =>
          val Type.Function(tpArgs) = meta.typ.typ
          meta.name -> tpArgs.last
        }
        .toMap

      val (checkedParams, paramsState) = params.foldMap { nextParam =>
        val (checkedParam, paramState) = gather(nextParam, env)
        val paramName                  = checkedParam.meta.name
        val paramType                  = checkedParam.meta.typ.typ
        // Emit a constraint that the type from our initial environment matches the ascription
        val paramAscribedCst =
          EqualType(paramType, envMemberTypes(paramName), checkedParam.meta.pos)
        (Chain.one(checkedParam), paramState.withConstraint(paramAscribedCst))
      }

      val paramTypes = checkedParams.toList.map(_.meta.typ.typ)

      val funType = TypeScheme(
        dataType.bound,
        Type.Function(paramTypes, dataType.typ)
      )

      val checkedConstr = constr.copy(
        params = checkedParams.toList,
        meta = meta.withType(funType)
      )

      (Chain.one(checkedConstr), paramsState)
  }

  def gather(
    decl: TopLevelDeclaration[Meta.Untyped],
    env: Environment[Meta.Typed]
  ): (Chain[TopLevelDeclaration[Meta.Typed]], State) = decl match {
    case let @ Let(name, expr, meta) =>
      val (checkedExpr, exprState) = gather(expr, env)

      // We have to solve the constraints under a `let` before we generalize it
      val Solve.State(_, subst, solveErrors) = Solve.solve(env, exprState.constraints)

      // Otherwise, the constraint substitution could not be applied to any bound type variables
      val solvedExpr = subst(checkedExpr)

      // Making sure to use the constraint solution...
      val solvedEnv = env.substituteTypes(subst.subst)

      // ...before generalizing our let binding
      val exprType = TypeScheme.generalize(
        solvedEnv,
        solvedExpr.meta.typ.typ
      )

      val checkedLet = Chain.one(
        let.copy(
          binding = solvedExpr,
          meta = meta.withType(exprType)
        )
      )

      val updatedEnv = solvedEnv
        .withType(name, exprType)

      val newState = State.empty
        .withConstraints(exprState.constraints.map(subst(_)))
        .withErrors(exprState.errors)
        .withErrors(solveErrors)
        .withEnv(updatedEnv)

      (checkedLet, newState)

    case data @ Data(name, typeParams, cases, meta) =>
      val tyVarMapping = typeParams.map(tyParam => tyParam.name -> TypeVariable(KindVariable()))
      val tyVarTypes   = tyVarMapping.map { case (nm, tv) => nm -> TypeScheme(tv) }
      val tyVarKinds   = tyVarMapping.map { case (nm, tv) => nm -> tv.kind }
      val tyVars       = tyVarMapping.map { case (_, tv) => tv }
      val dataConType  = TypeConstructor(name, KindVariable())
      val dataAppType  = if (tyVars.isEmpty) dataConType else TypeApply(dataConType, tyVars, Atomic)
      val dataType     = TypeScheme(List.empty, dataConType)

      val dataTypeEnv = env
        .withType(name, dataType)
        .withTypes(tyVarTypes)

      val (checkedTyParams, tyParamState) = typeParams.foldMap { nextParam =>
        val (param, paramState) = gather(nextParam, dataTypeEnv)
        (Chain.one(param), paramState)
      }

      val (checkedCases, casesState) = cases.foldMap { nextCase =>
        gather(nextCase, TypeScheme(tyVars, dataAppType), dataTypeEnv)
      }

      val checkedData = Chain.one(
        data.copy(
          typeParams = checkedTyParams.toList,
          cases = checkedCases.toList,
          meta = meta.withType(dataType)
        )
      )

      val constrNames = checkedCases.toList.map(_.meta.name)

      val constrMembers = checkedCases.toList.map { cse =>
        cse.meta.name -> cse.params.map { param =>
          val accessorFunType    = Type.Function(List(dataAppType), param.meta.typ.typ)
          val accessorTypeScheme = TypeScheme(tyVars, accessorFunType)
          param.meta.copy(typ = accessorTypeScheme)
        }
      }

      val envWithoutMembers = env.copy(
        members = env.members.removedAll(meta.name :: constrNames)
      )

      val envWithDataMembers = envWithoutMembers
        .withMembers(meta.name, checkedCases.toList.map(_.meta))
        .withType(name, dataType)
        .withTypes(checkedCases.map { cse => cse.name -> cse.meta.typ }.toList)
        .withKinds(tyVarKinds)

      val envWithConstrMembers = constrMembers.foldLeft(envWithDataMembers) {
        case (envSoFar, (name, members)) =>
          envSoFar.withMembers(name, members)
      }

      val newState = (tyParamState |+| casesState)
        .withEnv(envWithConstrMembers)

      (checkedData, newState.withEnv(envWithConstrMembers))
  }

  def initialPass(
    module: Module[Meta.Untyped],
    importedEnv: Environment[Meta.Typed]
  ): Environment[Meta.Typed] = {
    val symbolTable = module.symbolTable

    val initialEnv = importedEnv
      .withValueNames(symbolTable.valueNames)
      .withTypeNames(symbolTable.typeNames)

    val finalEnv = module.declarations.foldLeft(initialEnv) {
      case (envSoFar, Let(name, _, _)) =>
        envSoFar.withType(name, TypeScheme(TypeVariable()))

      case (envSoFar, Data(name, typeParams, cases, meta)) =>
        val tyVars      = typeParams.map(_ => TypeVariable(KindVariable()))
        val dataConType = TypeConstructor(name, KindVariable())
        val dataAppType =
          if (tyVars.isEmpty) dataConType else TypeApply(dataConType, tyVars, Atomic)
        val dataType = TypeScheme(List.empty, dataConType)

        val constrTypes = cases.map { case DataConstructor(constrName, params, _) =>
          val paramTypes = params.map(_ => TypeVariable())
          val constrType = TypeScheme(tyVars, Type.Function(paramTypes, dataAppType))
          constrName -> constrType
        }.toMap

        val dataMembers = cases.map { case DataConstructor(constrName, _, constrMeta) =>
          meta.name -> constrMeta.withType(constrTypes(constrName))
        }

        val constrMembers = cases.flatMap { case DataConstructor(_, params, constrMeta) =>
          params.map { case Param(_, _, paramMeta) =>
            val accessorFunType    = Type.Function(List(dataAppType), TypeVariable())
            val accessorTypeScheme = TypeScheme(tyVars, accessorFunType)
            constrMeta.name -> paramMeta.withType(accessorTypeScheme)
          }
        }

        val emptyConstructors = cases
          .filter(_.params.isEmpty)
          .map(constr => constr.meta.name -> List.empty[Meta.Typed])

        val allMembers = dataMembers ++ constrMembers

        val groupedMembers = allMembers.groupMap(_._1)(_._2) ++ emptyConstructors

        envSoFar
          .withType(name, dataType)
          .withTypes(constrTypes)
          .copy(members = envSoFar.members ++ groupedMembers)
    }

    finalEnv
  }

  def gather(
    module: Module[Meta.Untyped],
    importedEnv: Environment[Meta.Typed]
  ): (Module[Meta.Typed], State) = {
    val topLevelEnv = initialPass(module, importedEnv)

    val initialDeclState = (Chain.empty[TopLevelDeclaration[Meta.Typed]], State.init(topLevelEnv))
    val (decls, declState) = module.declarations.foldLeft(initialDeclState) {
      case (state @ (_, stateSoFar), nextDecl) =>
        state |+| gather(nextDecl, stateSoFar.env)
    }

    val Kindchecker.State(kindEnv, kindSubst, kindErrors, _) = decls
      .collect { case data @ Data(_, _, _, _) => data }
      .foldMap { data =>
        Kindchecker.kindcheck(data, declState.env)
      }

    val updatedMod = module
      .copy(
        declarations = decls.toList,
        meta = module.meta.withSimpleType(Type.Module)
      )
      .substituteKinds(kindSubst.subst)

    val updatedState = declState
      .substituteKinds(kindSubst)
      .withErrors(kindErrors)
      .withEnv(kindEnv)

    (updatedMod, updatedState)
  }
}
