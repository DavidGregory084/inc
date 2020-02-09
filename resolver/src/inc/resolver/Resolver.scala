package inc.resolver

import cats.data.Chain
import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.foldable._
import cats.syntax.traverse._
import inc.common._
import java.lang.String
import scala.{ Either, Right, StringContext }
import scala.collection.immutable.{ List, Map }

object Resolver {
  type SymbolTable = Map[String, Name]

  val EmptyTable: SymbolTable = Map.empty
  val EmptyResult: Either[List[ResolverError], Chain[Expr[Meta.Untyped]]] = Right(Chain.empty)

  def withName(expr: Expr[Pos], name: Name, tbl: SymbolTable) =
    Right((expr.map(pos => Meta.Untyped(name, pos)), tbl))

  def resolve(expr: Expr[Pos], tbl: SymbolTable): Either[List[ResolverError], (Expr[Meta.Untyped], SymbolTable)] = expr match {
    case int @ LiteralInt(_, _) =>
      withName(int, NoName, tbl)

    case long @ LiteralLong(_, _) =>
      withName(long, NoName, tbl)

    case float @ LiteralFloat(_, _) =>
      withName(float, NoName, tbl)

    case double @ LiteralDouble(_, _) =>
      withName(double, NoName, tbl)

    case bool @ LiteralBoolean(_, _) =>
      withName(bool, NoName, tbl)

    case char @ LiteralChar(_, _) =>
      withName(char, NoName, tbl)

    case str @ LiteralString(_, _) =>
      withName(str, NoName, tbl)

    case unit @ LiteralUnit(_) =>
      withName(unit, NoName, tbl)

    case ref @ Reference(_, name, pos)  =>
      tbl.get(ref.fullName)
        .map(nm => withName(ref, nm, tbl))
        .getOrElse(ResolverError.singleton(pos, s"Reference to undefined symbol: $name"))

    case If(cond, thenExpr, elseExpr, pos) =>
      for {
        (c, _) <- resolve(cond, tbl)
        (t, _) <- resolve(thenExpr, tbl)
        (e, _) <- resolve(elseExpr, tbl)
      } yield (If(c, t, e, Meta.Untyped(NoName, pos)), tbl)

    case Lambda(params, body, pos) =>
      // Allow name shadowing in lambda params
      val emptyTbl: SymbolTable = Map.empty
      val emptyParams: Chain[Param[Meta.Untyped]] = Chain.empty
      val emptyRes: Either[List[ResolverError], (Chain[Param[Meta.Untyped]], SymbolTable)] = Right((emptyParams, emptyTbl))

      val resolvedParams = params.foldLeft(emptyRes) {
        case (resSoFar, param @ Param(name, _, pos)) =>
          resSoFar.flatMap {
            case (paramsSoFar, updatedTbl) =>
              if (updatedTbl.contains(name))
                ResolverError.singleton(pos, s"Symbol $name is already defined")
              else {
                val localName = LocalName(name)
                val paramWithName = param.copy(meta = Meta.Untyped(localName, pos))
                Right((paramsSoFar :+ paramWithName, updatedTbl.updated(name, localName)))
              }
          }
      }

      for {
        (parms, paramTbl) <- resolvedParams
        (b, _) <- resolve(body, tbl ++ paramTbl)
      } yield (Lambda(parms.toList, b, Meta.Untyped(NoName, pos)), tbl)


    case Apply(fn, args, pos) =>
      for {
        (f, _) <- resolve(fn, tbl)

        ra <- args.foldLeft(EmptyResult) {
          case (resSoFar, nextArg) =>
            for {
              rs <- resSoFar
              (r, _) <- resolve(nextArg, tbl)
            } yield rs :+ r
        }

      } yield (Apply(f, ra.toList, Meta.Untyped(NoName, pos)), tbl)

    case Ascription(expr, ascribedAs, pos) =>
      resolve(expr, tbl).map {
        case (e, _) =>
          (Ascription(e, ascribedAs, Meta.Untyped(NoName, pos)), tbl)
      }
  }

  def resolve(mod: Module[Pos], decl: TopLevelDeclaration[Pos], tbl: SymbolTable): Either[List[ResolverError], (TopLevelDeclaration[Meta.Untyped], SymbolTable)] = decl match {
    case Let(name, expr, pos) =>
      val memberName = MemberName(mod.pkg, mod.name, name)
      resolve(expr, tbl).flatMap {
        case (resolvedExpr, updatedTbl) =>
          Right((Let(name, resolvedExpr, Meta.Untyped(memberName, pos)), updatedTbl))
      }

    case data @ Data(_, _, cases, pos) =>
      val resolvedCases = cases.traverse {
        case constr @ DataConstructor(_, params, _, constrPos) =>
          val emptyTbl: SymbolTable = Map.empty
          val emptyParams: Chain[Param[Meta.Untyped]] = Chain.empty
          val emptyRes: Either[List[ResolverError], (Chain[Param[Meta.Untyped]], SymbolTable)] = Right((emptyParams, emptyTbl))

          val resolvedParams = params.foldLeft(emptyRes) {
            case (resSoFar, param @ Param(name, _, pos)) =>
              resSoFar.flatMap {
                case (paramsSoFar, updatedTbl) =>
                  if (updatedTbl.contains(name))
                    ResolverError.singleton(pos, s"Symbol $name is already defined")
                  else {
                    val localName = LocalName(name)
                    val paramWithName = param.copy(meta = Meta.Untyped(localName, pos))
                    Right((paramsSoFar :+ paramWithName, updatedTbl.updated(name, localName)))
                  }
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

        val updatedTbl = cses.foldLeft(tbl) {
          case (tbl, nextCase) =>
            tbl.updated(nextCase.name, nextCase.meta.name)
        }

        (updatedData, updatedTbl)
      }
  }

  def resolve(
    module: Module[Pos],
    importedEnv: Environment = Environment.empty
  ): Either[List[ResolverError], Module[Meta.Untyped]] = module match {
    case Module(pkg, name, _, decls, pos) =>
      val importedTbl = importedEnv.names

      // Do an initial pass over the top level declarations
      val initialRes = decls.foldLeft(importedTbl.asRight[List[ResolverError]]) {
        case (resSoFar, Let(name, _, pos)) =>
          resSoFar.flatMap { tbl =>
            if (tbl.contains(name))
              ResolverError.singleton(pos, s"Symbol $name is already defined")
            else
              Right(tbl.updated(name, MemberName(module.pkg, module.name, name)))
          }
        case (resSoFar, Data(dataName, _, cases, _)) =>
          resSoFar.flatMap { outerTbl =>
            cases.foldM(outerTbl.updated(dataName, DataName(module.pkg, module.name, dataName))) {
              case (tbl, DataConstructor(name, _, _, pos)) =>
                if (tbl.contains(name))
                  ResolverError.singleton(pos, s"Symbol $name is already defined")
                else
                  Right(tbl.updated(name, ConstrName(module.pkg, module.name, dataName, name)))
            }
          }
      }

      // Resolve names within the bodies of the declarations
      val emptyDecls = Chain.empty[TopLevelDeclaration[Meta.Untyped]]

      val resolvedDecls = decls.foldLeft(initialRes.map(tbl => (emptyDecls, tbl))) {
        case (resSoFar, nextDecl) =>
          for {
            (resolvedSoFar, tblSoFar) <- resSoFar
            (resolvedDecl, updatedTbl) <- resolve(module, nextDecl, tblSoFar)
          } yield (resolvedSoFar :+ resolvedDecl, updatedTbl)
      }

      resolvedDecls.map {
        case (resolved, _) =>
          module.copy(
            declarations = resolved.toList,
            meta = Meta.Untyped(ModuleName(pkg, name), pos)
          )
      }
  }
}
