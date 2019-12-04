package inc.resolver

import cats.data.Chain
import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.traverse._
import inc.common._
import java.lang.String
import scala.{ Either, Right, StringContext }
import scala.collection.immutable.{ List, Map }

object Resolver {
  type SymbolTable = Map[String, Name]

  val EmptyTable: SymbolTable = Map.empty
  val EmptyResult: Either[List[ResolverError], Chain[Expr[NameWithPos]]] = Right(Chain.empty)

  def withName(expr: Expr[Pos], name: Name, tbl: SymbolTable) =
    Right((expr.map(pos => NameWithPos(name, pos)), tbl))

  def resolve(expr: Expr[Pos], tbl: SymbolTable): Either[List[ResolverError], (Expr[NameWithPos], SymbolTable)] = expr match {
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
      } yield (If(c, t, e, NameWithPos(NoName, pos)), tbl)

    case Lambda(params, body, pos) =>
      // Allow name shadowing in lambda params
      val tblWithoutParams = tbl.filterNot { case (nm, _) => params.map(_.name).contains(nm) }
      val emptyParams: Chain[Param[NameWithPos]] = Chain.empty
      val emptyRes: Either[List[ResolverError], (Chain[Param[NameWithPos]], SymbolTable)] = Right((emptyParams, tblWithoutParams))

      val resolvedParams = params.foldLeft(emptyRes) {
        case (resSoFar, param @ Param(name, _, pos)) =>
          resSoFar.flatMap {
            case (paramsSoFar, updatedTbl) =>
              if (updatedTbl.contains(name))
                ResolverError.singleton(pos, s"Symbol $name is already defined")
              else {
                val localName = LocalName(name)
                val paramWithName = param.copy(meta = NameWithPos(localName, pos))
                Right((paramsSoFar :+ paramWithName, updatedTbl.updated(name, localName)))
              }
          }
      }

      for {
        (parms, updatedTbl) <- resolvedParams
        (b, _) <- resolve(body, updatedTbl)
      } yield (Lambda(parms.toList, b, NameWithPos(NoName, pos)), tbl)


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

      } yield (Apply(f, ra.toList, NameWithPos(NoName, pos)), tbl)

    case Ascription(expr, ascribedAs, pos) =>
      resolve(expr, tbl).map {
        case (e, _) =>
          (Ascription(e, ascribedAs, NameWithPos(NoName, pos)), tbl)
      }
  }

  def resolve(mod: Module[Pos], decl: TopLevelDeclaration[Pos], tbl: SymbolTable): Either[List[ResolverError], (TopLevelDeclaration[NameWithPos], SymbolTable)] = decl match {
    case Let(name, expr, pos) =>
      val memberName = MemberName(mod.pkg, mod.name, name)
      resolve(expr, tbl).flatMap {
        case (resolvedExpr, updatedTbl) =>
          Right((Let(name, resolvedExpr, NameWithPos(memberName, pos)), updatedTbl))
      }

    case data @ Data(name, _, cases, pos) =>
      val resolvedCases = cases.map {
        case constr @ DataConstructor(caseName, params, _, casePos) =>
          val resolvedParams = params.map { param =>
            param.map(pos => NameWithPos(LocalName(param.name), pos))
          }
          val memberName = MemberName(mod.pkg, mod.name, caseName)
          constr.copy(params = resolvedParams, meta = NameWithPos(memberName, casePos))
      }

      val updatedData = data.copy(
        cases = resolvedCases,
        meta = NameWithPos(NoName, pos)
      )

      val updatedTbl = resolvedCases.foldLeft(tbl) {
        case (tbl, nextCase) =>
          tbl.updated(name, nextCase.meta.name)
      }

      Right((updatedData, updatedTbl))
  }

  def resolve(
    module: Module[Pos],
    importedDecls: Map[String, TopLevelDeclaration[NameWithType]] = Map.empty
  ): Either[List[ResolverError], Module[NameWithPos]] = module match {
    case Module(pkg, name, _, decls, pos) =>
      val importedTbl = importedDecls.view.map { case (sym, tld) => (sym, tld.meta.name) }.toMap

      // Do an initial pass over the top level declarations
      val initialRes = decls.foldLeft(importedTbl.asRight[List[ResolverError]]) {
        case (resSoFar, Let(name, _, pos)) =>
          resSoFar.flatMap { tbl =>
            if (tbl.contains(name))
              ResolverError.singleton(pos, s"Symbol $name is already defined")
            else
              Right(tbl.updated(name, MemberName(module.pkg, module.name, name)))
          }
        case (resSoFar, Data(_, _, cases, _)) =>
          resSoFar.flatMap { tbl =>
            cases.traverse {
              case DataConstructor(name, _, _, pos) =>
                if (tbl.contains(name))
                  ResolverError.singleton(pos, s"Symbol $name is already defined")
                else
                  Right(tbl.updated(name, MemberName(module.pkg, module.name, name)))
            }.map { tbls => tbls.foldLeft(Map.empty: SymbolTable)(_ ++ _) }
          }
      }

      // Resolve names within the bodies of the declarations
      val emptyDecls = Chain.empty[TopLevelDeclaration[NameWithPos]]

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
            meta = NameWithPos(ModuleName(pkg, name), pos)
          )
      }
  }
}
