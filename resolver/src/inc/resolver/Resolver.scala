package inc.resolver

import cats.data.Chain
import cats.instances.either._
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.foldable._
import cats.syntax.traverse._
import inc.common._
import scala.{ Right, Option, Some, None, StringContext }
import scala.collection.immutable.List
import scala.Predef.ArrowAssoc

object Resolver {
  type SymbolTable = Environment[Meta.Untyped]
  type Decl = TopLevelDeclaration[Meta.Untyped]
  val EmptyTable: SymbolTable = Environment.empty[Meta.Untyped]
  val EmptyResult: Resolve[Chain[Expr[Meta.Untyped]]] = Right(Chain.empty)

  def withName(expr: Expr[Pos], name: Name, env: SymbolTable) =
    Right((expr.map(pos => Meta.Untyped(name, pos)), env))

  def resolve(pattern: Pattern[Pos], enclosingName: Option[ConstrName], env: SymbolTable): Resolve[(Pattern[Meta.Untyped], SymbolTable)] = pattern match {
    case IdentPattern(name, pos) =>
      if (env.names.contains(name))
        ResolverError.singleton(pos, s"Symbol $name is already defined")
      else {
        val localName = LocalName(name)

        val resolvedId = enclosingName.flatMap { constrName =>
          env.members.get(constrName).map { members =>
            (constrName, members)
          }
        }.map {
          case (constrName, members) =>
            if (members.exists(_.name == localName))
              Right((IdentPattern(name, Meta.Untyped(localName, pos)), env.withName(name, localName)))
            else
              ResolverError.singleton(pos, s"Constructor ${constrName.name} has no member ${name}")
        }

        resolvedId.getOrElse {
          Right((IdentPattern(name, Meta.Untyped(localName, pos)), env.withName(name, localName)))
        }
      }

    case AliasPattern(idPat @ IdentPattern(_, idPos), alias, pos) =>
      if (env.names.contains(alias))
        ResolverError.singleton(pos, s"Symbol $alias is already defined")
      else {
        // There's no need to check idPat as it's being aliased away
        val localName = LocalName(alias)
        val resolvedId = idPat.copy(meta = Meta.Untyped(NoName, idPos))
        Right((AliasPattern(resolvedId, alias, Meta.Untyped(localName, pos)), env.withName(alias, localName)))
      }

    case AliasPattern(pattern, alias, pos) =>
      if (env.names.contains(alias))
        ResolverError.singleton(pos, s"Symbol $alias is already defined")
      else resolve(pattern, enclosingName, env).map {
        case (pat, patEnv) =>
          val localName = LocalName(alias)
          (AliasPattern(pat, alias, Meta.Untyped(localName, pos)), patEnv.withName(alias, localName))
      }

    case ConstrPattern(name, patterns, pos) =>
      env.names.get(name).collectFirst {
        case constrNm @ ConstrName(_, _, _, _) =>
          val emptyPatterns = Chain.empty[Pattern[Meta.Untyped]]
          patterns.foldM((emptyPatterns, env)) {
            case ((patsSoFar, envSoFar), nextPat) =>
              resolve(nextPat, Some(constrNm), envSoFar).map {
                case (resolvedPat, patEnv) =>
                  (patsSoFar :+ resolvedPat, patEnv)
              }
          }.map {
            case (resolvedPats, patEnv) =>
            (ConstrPattern(name, resolvedPats.toList, Meta.Untyped(NoName, pos)), patEnv)
          }
      }.getOrElse(ResolverError.singleton(pos, s"Reference to unknown constructor: $name"))
  }

  def resolve(matchCase: MatchCase[Pos], env: SymbolTable): Resolve[MatchCase[Meta.Untyped]] = matchCase match {
    case MatchCase(pattern, result, pos) =>
      for {
        (pat, patEnv) <- resolve(pattern, None, env)
        (res, _) <- resolve(result, patEnv)
      } yield MatchCase(pat, res, Meta.Untyped(NoName, pos))
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

    case ref @ Reference(_, name, pos)  =>
      env.names.get(ref.fullName)
        .map(nm => withName(ref, nm, env))
        .getOrElse(ResolverError.singleton(pos, s"Reference to undefined symbol: $name"))

    case If(cond, thenExpr, elseExpr, pos) =>
      for {
        (c, _) <- resolve(cond, env)
        (t, _) <- resolve(thenExpr, env)
        (e, _) <- resolve(elseExpr, env)
      } yield (If(c, t, e, Meta.Untyped(NoName, pos)), env)

    case Lambda(params, body, pos) =>
      // Allow name shadowing in lambda params
      val emptyParams = Chain.empty[Param[Meta.Untyped]]
      val emptyRes = ((emptyParams, Environment.empty[Meta.Untyped]))

      val resolvedParams = params.foldM(emptyRes) {
        case ((paramsSoFar, updatedEnv), param @ Param(name, _, pos)) =>
          if (updatedEnv.names.contains(name))
            ResolverError.singleton(pos, s"Symbol $name is already defined")
          else {
            val localName = LocalName(name)
            val paramWithName = param.copy(meta = Meta.Untyped(localName, pos))
            Right((paramsSoFar :+ paramWithName, updatedEnv.withName(name, localName)))
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
      resolve(expr, env).map {
        case (e, _) =>
          (Ascription(e, ascribedAs, Meta.Untyped(NoName, pos)), env)
      }

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

    case data @ Data(_, _, cases, pos) =>
      val resolvedCases = cases.traverse {
        case constr @ DataConstructor(_, params, _, constrPos) =>
          val emptyParams: Chain[Param[Meta.Untyped]] = Chain.empty
          val emptyRes = (emptyParams, Environment.empty[Meta.Untyped])

          val resolvedParams = params.foldM(emptyRes) {
            case ((paramsSoFar, updatedEnv), param @ Param(name, _, pos)) =>
                if (updatedEnv.names.contains(name))
                  ResolverError.singleton(pos, s"Symbol $name is already defined")
                else {
                  val localName = LocalName(name)
                  val paramWithName = param.copy(meta = Meta.Untyped(localName, pos))
                  Right((paramsSoFar :+ paramWithName, updatedEnv.withName(name, localName)))
                }
          }

          resolvedParams.map {
            case (parms, _) =>
              val constrName = ConstrName(mod.pkg, mod.name, data.name, constr.name)
              constr.copy(params = parms.toList, meta = Meta.Untyped(constrName, constrPos))
          }
      }

      resolvedCases.map { cses =>
        val updatedData = data.copy(
          cases = cses,
          meta = Meta.Untyped(DataName(mod.pkg, mod.name, data.name), pos)
        )

        val updatedEnv = cses.foldLeft(env) {
          case (envSoFar, nextCase) =>
            envSoFar.withName(nextCase.name, nextCase.meta.name)
        }

        (updatedData, updatedEnv)
      }
  }

  def initialPass(
    module: Module[Pos],
    decls: List[TopLevelDeclaration[Pos]],
    importedEnv: SymbolTable
  ): Resolve[SymbolTable] = {
    decls.foldM(importedEnv) {
      case (env, Let(name, _, pos)) =>
        if (env.names.contains(name))
          ResolverError.singleton(pos, s"Symbol $name is already defined")
        else
          Right(env.withName(name, MemberName(module.pkg, module.name, name)))
      case (outerEnv, Data(dataNm, _, cases, _)) =>
        val dataName = DataName(module.pkg, module.name, dataNm)
        val dataEnv = outerEnv.withName(dataNm, dataName)

        val constrEnv = cases.foldM(dataEnv){
          case (env, DataConstructor(name, _, _, pos)) =>
            if (env.names.contains(name))
              ResolverError.singleton(pos, s"Symbol $name is already defined")
            else
              Right(env.withName(name, ConstrName(module.pkg, module.name, dataNm, name)))
        }

        constrEnv.map { env =>
          val dataMembers = for {
            DataConstructor(name, params, _, pos) <- cases
            constrName = ConstrName(module.pkg, module.name, dataNm, name)
            dataMember = dataName -> Meta.Untyped(constrName, pos)
          } yield dataMember

          val constrMembers = for {
            DataConstructor(name, params, _, _) <- cases
            constrName = ConstrName(module.pkg, module.name, dataNm, name)
            Param(paramNm, _, pos) <- params
            paramName = LocalName(paramNm)
            constrMember = constrName -> Meta.Untyped(paramName, pos)
          } yield constrMember

          val allMembers = dataMembers ++ constrMembers

          val memberMap = allMembers.groupMap(_._1)(_._2)

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
