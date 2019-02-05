package inc.resolver

import cats.data.Chain
import cats.syntax.functor._
import inc.common._
import java.lang.String
import scala.{ Either, Right, StringContext, Unit }
import scala.collection.immutable.{ List, Map }

object Resolver {
  type SymbolTable = Map[String, Name]

  val EmptyTable: SymbolTable = Map.empty
  val EmptyResult: Either[List[ResolverError], Chain[Expr[Name]]] = Right(Chain.empty)

  def withName(expr: Expr[Unit], name: Name, tbl: SymbolTable) =
    Right((expr.as(name), tbl))

  def resolve(expr: Expr[Unit], tbl: SymbolTable): Either[List[ResolverError], (Expr[Name], SymbolTable)] = expr match {
    case int @ LiteralInt(_, _, _) =>
      withName(int, NoName, tbl)

    case long @ LiteralLong(_, _, _) =>
      withName(long, NoName, tbl)

    case float @ LiteralFloat(_, _, _) =>
      withName(float, NoName, tbl)

    case double @ LiteralDouble(_, _, _) =>
      withName(double, NoName, tbl)

    case bool @ LiteralBoolean(_, _, _) =>
      withName(bool, NoName, tbl)

    case char @ LiteralChar(_, _, _) =>
      withName(char, NoName, tbl)

    case str @ LiteralString(_, _, _) =>
      withName(str, NoName, tbl)

    case unit @ LiteralUnit(_, _) =>
      withName(unit, NoName, tbl)

    case ref @ Reference(name, pos, _)  =>
      tbl.get(name)
        .map(nm => withName(ref, nm, tbl))
        .getOrElse(ResolverError.singleton(pos, s"Reference to undefined symbol: $name"))

    case If(cond, thenExpr, elseExpr, pos, _) =>
      for {
        (c, _) <- resolve(cond, tbl)
        (t, _) <- resolve(thenExpr, tbl)
        (e, _) <- resolve(elseExpr, tbl)
      } yield (If(c, t, e, pos, NoName), tbl)

    case Lambda(params, body, pos, _) =>
      // Allow name shadowing in lambda params
      val tblWithoutParams = tbl.filterNot { case (nm, _) => params.map(_.name).contains(nm) }
      val emptyParams: Chain[Param[Name]] = Chain.empty
      val emptyRes: Either[List[ResolverError], (Chain[Param[Name]], SymbolTable)] = Right((emptyParams, tblWithoutParams))

      val resolvedParams = params.foldLeft(emptyRes) {
        case (resSoFar, param @ Param(name, pos, _)) =>
          resSoFar.flatMap {
            case (paramsSoFar, updatedTbl) =>
              if (updatedTbl.contains(name))
                ResolverError.singleton(pos, s"Symbol $name is already defined")
              else {
                val localName: Name = LocalName(name)
                val paramWithName = param.copy(meta = localName)
                Right((paramsSoFar :+ paramWithName, updatedTbl.updated(name, localName)))
              }
          }
      }

      for {
        (parms, updatedTbl) <- resolvedParams
        (b, _) <- resolve(body, updatedTbl)
      } yield (Lambda(parms.toList, b, pos, NoName), tbl)


    case Apply(fn, args, pos, _) =>
      for {
        (f, _) <- resolve(fn, tbl)

        ra <- args.foldLeft(EmptyResult) {
          case (resSoFar, nextArg) =>
            for {
              rs <- resSoFar
              (r, _) <- resolve(nextArg, tbl)
            } yield rs :+ r
        }

      } yield (Apply(f, ra.toList, pos, NoName), tbl)
  }

  def resolve(mod: Module[Unit], decl: TopLevelDeclaration[Unit], tbl: SymbolTable): Either[List[ResolverError], (TopLevelDeclaration[Name], SymbolTable)] = decl match {
    case Let(name, expr, pos, _) =>
      val memberName = MemberName(mod.pkg, mod.name, name)

      resolve(expr, tbl).flatMap {
        case (resolvedExpr, updatedTbl) =>
          if (updatedTbl.contains(name))
            ResolverError.singleton(resolvedExpr.pos, s"Symbol $name is already defined")
          else
            Right((Let(name, resolvedExpr, pos, memberName), updatedTbl.updated(name, memberName)))
      }
  }

  def resolve(
    module: Module[Unit],
    importedDecls: Map[String, TopLevelDeclaration[NameWithType]] = Map.empty
  ): Either[List[ResolverError], Module[Name]] = module match {
    case Module(pkg, name, _, decls, _, _) =>
      val initialTbl = importedDecls.mapValues(_.meta.name)

      val emptyDecls = Chain.empty[TopLevelDeclaration[Name]]
      val emptyRes: Either[List[ResolverError], (Chain[TopLevelDeclaration[Name]], SymbolTable)] = Right((emptyDecls, initialTbl))

      val resolvedDecls = decls.foldLeft(emptyRes) {
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
            meta = ModuleName(pkg, name)
          )
      }
  }
}
