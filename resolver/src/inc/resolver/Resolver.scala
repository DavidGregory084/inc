package inc.resolver

import cats.data.Chain
import cats.syntax.functor._
import cats.syntax.foldable._
import cats.syntax.traverse._
import inc.common._
import scala.{ Right, Option, Some, None }
import scala.collection.immutable.List
import scala.Predef.ArrowAssoc

object Resolver {
  type SymbolTable = Environment[Meta.Untyped]
  type Decl = TopLevelDeclaration[Meta.Untyped]
  val EmptyTable: SymbolTable = Environment.empty[Meta.Untyped]
  val EmptyResult: Resolve[Chain[Expr[Meta.Untyped]]] = Right(Chain.empty)

  def withName(expr: Expr[Pos], name: Name, env: SymbolTable) =
    Right((expr.map(pos => Meta.Untyped(name, pos)), env))

  def resolve(pattern: Pattern[Pos], env: SymbolTable): Resolve[(Pattern[Meta.Untyped], SymbolTable)] = pattern match {
    case IdentPattern(name, pos) =>
        // Allow shadowing in patterns
        val localName = LocalName(name)
        Right((IdentPattern(name, Meta.Untyped(localName, pos)), env.withValueName(name, localName)))

    case ConstrPattern(name, alias, patterns, pos) =>
      env.valueNames.get(name).collectFirst {
        case constrNm @ ConstrName(_, _, _, _) =>
          val emptyPatterns = Chain.empty[FieldPattern[Meta.Untyped]]
          val members = env.members(constrNm)
          patterns.foldM((emptyPatterns, env)) {
            case ((patsSoFar, envSoFar), FieldPattern(field, None, pos)) =>
              val fieldName = LocalName(field)
              if (!members.exists(_.name == fieldName))
                ResolverError.unknownMember(pos, constrNm.name, name)
              else
                Right((patsSoFar :+ FieldPattern(field, Option.empty[Pattern[Meta.Untyped]], Meta.Untyped(fieldName, pos)), envSoFar.withValueName(field, fieldName)))
            case ((patsSoFar, envSoFar), FieldPattern(field, Some(nextPat), pos)) =>
              val fieldName = LocalName(field)
              if (!members.exists(_.name == fieldName))
                ResolverError.unknownMember(pos, constrNm.name, name)
              else resolve(nextPat, envSoFar).map {
                case (resolvedPat, patEnv) =>
                  (patsSoFar :+ FieldPattern(field, Some(resolvedPat), Meta.Untyped(fieldName, pos)), patEnv)
              }
          }.map {
            case (resolvedPats, patEnv) =>
              val aliasName = alias.map(LocalName.apply).getOrElse(NoName)
              val aliasEnv = alias.map(patEnv.withValueName(_, aliasName)).getOrElse(patEnv)
              (ConstrPattern(name, alias, resolvedPats.toList, Meta.Untyped(aliasName, pos)), aliasEnv)
          }
      }.getOrElse(ResolverError.unknownConstructor(pos, name))
  }

  def resolve(matchCase: MatchCase[Pos], env: SymbolTable): Resolve[MatchCase[Meta.Untyped]] = matchCase match {
    case MatchCase(pattern, result, pos) =>
      for {
        (pat, patEnv) <- resolve(pattern, env)
        (res, _) <- resolve(result, patEnv)
      } yield MatchCase(pat, res, Meta.Untyped(NoName, pos))
  }

  def resolve(expr: TypeConstructorExpr[Pos], env: SymbolTable): Resolve[(TypeConstructorExpr[Meta.Untyped], SymbolTable)] =
    expr match {
      case TypeConstructorExpr(_, _, pos) =>
        env.typeNames.get(expr.fullName)
          .map(nm => Right((expr.copy(meta = Meta.Untyped(nm, pos)), env)))
          .getOrElse(ResolverError.undefined(pos, expr.fullName))
    }

  def resolve(expr: TypeExpr[Pos], env: SymbolTable): Resolve[(TypeExpr[Meta.Untyped], SymbolTable)] = expr match {
    case TypeApplyExpr(typ, args, pos) =>
      val emptyArgs = Chain.empty[TypeExpr[Meta.Untyped]]
      val emptyRes = ((emptyArgs, env))

      for {
        (resolvedTyp, _) <- resolve(typ, env)

        (resolvedArgs, _) <- args.foldM(emptyRes) {
          case ((argsSoFar, updatedEnv), arg) =>
            resolve(arg, updatedEnv).map {
              case (arg, argEnv) =>
                (argsSoFar:+ arg, updatedEnv ++ argEnv)
            }
        }

      } yield (TypeApplyExpr(resolvedTyp, resolvedArgs.toList, Meta.Untyped(NoName, pos)), env)

    case tyCon @ TypeConstructorExpr(_, _, _) =>
      resolve(tyCon, env)
  }

  def resolve(param: Param[Pos], env: SymbolTable): Resolve[(Param[Meta.Untyped], SymbolTable)] = param match {
    case Param(name, ascribedAs, pos) =>
      if (env.valueNames.contains(name))
        ResolverError.alreadyDefined(pos, name, env.valueNames(name))
      else if (env.typeNames.contains(name))
        ResolverError.alreadyDefined(pos, name, env.typeNames(name))
      else ascribedAs match {
        case Some(ascription) =>
          resolve(ascription, env).map {
            case (resolvedAscription, _) =>
              val localName = LocalName(name)

              val paramWithName = param.copy(
                ascribedAs = Some(resolvedAscription),
                meta = Meta.Untyped(localName, pos))

              (paramWithName, env.withValueName(name, localName))
          }
        case None =>
          val localName = LocalName(name)

          val paramWithName = param.copy(
            ascribedAs = Option.empty,
            meta = Meta.Untyped(localName, pos))

          Right((paramWithName, env.withValueName(name, localName)))
      }
  }

  def resolve(expr: Expr[Pos], env: SymbolTable): Resolve[(Expr[Meta.Untyped], SymbolTable)] = expr match {
    case int @ LiteralInt(_, _) =>
      withName(int, NoName, env)

    case long @ LiteralLong(_, _) =>
      withName(long, NoName, env)

    case float @ LiteralFloat(_, _) =>
      withName(float, NoName, env)

    case double @ LiteralDouble(_, _) =>
      withName(double, NoName, env)

    case bool @ LiteralBoolean(_, _) =>
      withName(bool, NoName, env)

    case char @ LiteralChar(_, _) =>
      withName(char, NoName, env)

    case str @ LiteralString(_, _) =>
      withName(str, NoName, env)

    case unit @ LiteralUnit(_) =>
      withName(unit, NoName, env)

    case ref @ Reference(_, _, pos)  =>
      env.valueNames.get(ref.fullName)
        .map(nm => withName(ref, nm, env))
        .getOrElse(ResolverError.undefined(pos, ref.fullName))

    case If(cond, thenExpr, elseExpr, pos) =>
      for {
        (c, _) <- resolve(cond, env)
        (t, _) <- resolve(thenExpr, env)
        (e, _) <- resolve(elseExpr, env)
      } yield (If(c, t, e, Meta.Untyped(NoName, pos)), env)

    case Lambda(params, body, pos) =>
      // Allow name shadowing in lambda params
      val emptyParams = Chain.empty[Param[Meta.Untyped]]
      val emptyRes = ((emptyParams, Environment.empty[Meta.Untyped].withTypeNames(env.typeNames)))

      val resolvedParams = params.foldM(emptyRes) {
        case ((paramsSoFar, updatedEnv), param) =>
          resolve(param, updatedEnv).map {
            case (param, paramEnv) =>
              (paramsSoFar :+ param, updatedEnv ++ paramEnv)
          }
      }

      for {
        (parms, paramEnv) <- resolvedParams
        (b, _) <- resolve(body, env ++ paramEnv)
      } yield (Lambda(parms.toList, b, Meta.Untyped(NoName, pos)), env)


    case Apply(fn, args, pos) =>
      for {
        (f, _) <- resolve(fn, env)

        ra <- args.foldM(Chain.empty[Expr[Meta.Untyped]]) {
          case (argsSoFar, nextArg) =>
             resolve(nextArg, env).map {
               case (arg, _) =>
                argsSoFar :+ arg
             }
        }

      } yield (Apply(f, ra.toList, Meta.Untyped(NoName, pos)), env)

    case Ascription(expr, ascribedAs, pos) =>
      for {
        (e, _) <- resolve(expr, env)
        (a, _) <- resolve(ascribedAs, env)
      } yield (Ascription(e, a, Meta.Untyped(NoName, pos)), env)

    case Match(expr, cases, pos) =>
      resolve(expr, env).flatMap {
        case (m, _) =>
          cases.traverse {
            resolve(_, env)
          }.map { cs =>
            (Match(m, cs, Meta.Untyped(NoName, pos)), env)
          }
      }
  }

  def resolve(mod: Module[Pos], decl: TopLevelDeclaration[Pos], env: SymbolTable): Resolve[(TopLevelDeclaration[Meta.Untyped], SymbolTable)] = decl match {
    case Let(name, expr, pos) =>
      val memberName = MemberName(mod.pkg, mod.name, name)
      resolve(expr, env).flatMap {
        case (resolvedExpr, updatedEnv) =>
          Right((Let(name, resolvedExpr, Meta.Untyped(memberName, pos)), updatedEnv))
      }

    case data @ Data(_, tparams, cases, pos) =>
      val emptyRes = ((Chain.empty[TypeConstructorExpr[Meta.Untyped]], env))

      for {
        (resolvedTparams, tparamEnv) <- tparams.foldM(emptyRes) {
          case ((tparamsSoFar, updatedEnv), tparam @ TypeConstructorExpr(_, _, _)) =>
            val tyVarEnv = updatedEnv.withTypeName(tparam.name, LocalName(tparam.name))

            resolve(tparam, tyVarEnv).map {
              case (tp, _) =>
                (tparamsSoFar :+ tp, tyVarEnv)
            }
        }

        resolvedCases <- cases.traverse {
          case constr @ DataConstructor(_, params, constrPos) =>
            val emptyParams: Chain[Param[Meta.Untyped]] = Chain.empty
            val emptyRes = (emptyParams, EmptyTable.withTypeNames(tparamEnv.typeNames))

            val resolvedParams = params.foldM(emptyRes) {
              case ((paramsSoFar, updatedEnv), param) =>
                resolve(param, updatedEnv).map {
                  case (param, paramEnv) =>
                    (paramsSoFar :+ param, updatedEnv ++ paramEnv)
                }
            }

            resolvedParams.map {
              case (parms, _) =>
                val constrName = ConstrName(mod.pkg, mod.name, data.name, constr.name)
                constr.copy(params = parms.toList, meta = Meta.Untyped(constrName, constrPos))
            }
        }

        updatedData = data.copy(
          typeParams = resolvedTparams.toList,
          cases = resolvedCases,
          meta = Meta.Untyped(DataName(mod.pkg, mod.name, data.name), pos)
        )

        updatedEnv = resolvedCases.foldLeft(env) {
          case (envSoFar, nextCase) =>
            envSoFar.withValueName(nextCase.name, nextCase.meta.name)
        }

      } yield (updatedData, updatedEnv)
  }

  def initialPass(
    module: Module[Pos],
    decls: List[TopLevelDeclaration[Pos]],
    importedEnv: SymbolTable
  ): Resolve[SymbolTable] = {
    decls.foldM(importedEnv) {
      case (env, Let(name, _, pos)) =>
        if (env.valueNames.contains(name))
          ResolverError.alreadyDefined(pos, name, env.valueNames(name))
        else if (env.typeNames.contains(name))
          ResolverError.alreadyDefined(pos, name, env.typeNames(name))
        else
          Right(env.withValueName(name, MemberName(module.pkg, module.name, name)))
      case (outerEnv, Data(dataNm, _, cases, dataPos)) =>
        val dataName = DataName(module.pkg, module.name, dataNm)
        val dataEnv = outerEnv.withTypeName(dataNm, dataName)

        val constrEnv = cases.foldM(dataEnv){
          case (env, DataConstructor(name, _, pos)) =>
            if (env.valueNames.contains(name))
              ResolverError.alreadyDefined(pos, name, env.valueNames(name))
            else
              Right(env.withValueName(name, ConstrName(module.pkg, module.name, dataNm, name)))
        }

        if (outerEnv.typeNames.contains(dataNm))
          ResolverError.alreadyDefined(dataPos, dataNm, outerEnv.typeNames(dataNm))
        else constrEnv.map { env =>
          val dataMembers = for {
            DataConstructor(name, params, pos) <- cases
            constrName = ConstrName(module.pkg, module.name, dataNm, name)
            dataMember = dataName -> Meta.Untyped(constrName, pos)
          } yield dataMember

          val constrMembers = for {
            DataConstructor(name, params, _) <- cases
            constrName = ConstrName(module.pkg, module.name, dataNm, name)
            Param(paramNm, _, pos) <- params
            paramName = LocalName(paramNm)
            constrMember = constrName -> Meta.Untyped(paramName, pos)
          } yield constrMember

          val emptyConstrs = for {
            DataConstructor(name, params, _) <- cases
            constrName = ConstrName(module.pkg, module.name, dataNm, name)
            if params.isEmpty
          } yield constrName -> List.empty[Meta.Untyped]

          val allMembers = dataMembers ++ constrMembers

          val memberMap = allMembers.groupMap(_._1)(_._2) ++ emptyConstrs

          env.copy(members = env.members ++ memberMap)
        }
    }
  }

  def resolve(
    module: Module[Pos],
    importedEnv: SymbolTable = Environment.empty
  ): Resolve[Module[Meta.Untyped]] = module match {
    case Module(pkg, name, _, decls, pos) =>
      // Do an initial pass over the top level declarations
      initialPass(module, decls, importedEnv).flatMap { initialEnv =>
        // Resolve names within the bodies of the declarations
        val emptyDecls = Chain.empty[Decl]
        decls.foldM((emptyDecls, initialEnv)) {
          case ((resolvedSoFar, envSoFar), nextDecl) =>
            resolve(module, nextDecl, envSoFar).map {
              case (resolvedDecl, updatedEnv) =>
                (resolvedSoFar :+ resolvedDecl, updatedEnv)
            }
        }.map {
          case (resolved, _) =>
            module.copy(
              declarations = resolved.toList,
              meta = Meta.Untyped(ModuleName(pkg, name), pos)
            )
        }
      }
  }
}
