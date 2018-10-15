package inc.resolver

import cats.data.Chain
import cats.syntax.functor._
import inc.common._

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

    case ref @ Reference(name, pos)  =>
      tbl.get(name)
        .map(nm => withName(ref, nm, tbl))
        .getOrElse(ResolverError.singleton(pos, s"Reference to undefined symbol: $name"))

    case If(cond, thenExpr, elseExpr, pos) =>
      for {
        r1 <- resolve(cond, tbl)
        (c, _) = r1
        r2 <- resolve(thenExpr, tbl)
        (t, _) = r2
        r3 <- resolve(elseExpr, tbl)
        (e, _) = r3
      } yield (If(c, t, e, NameWithPos(NoName, pos)), tbl)

    case Lambda(params, body, pos) =>
      // Allow name shadowing in lambda params
      val tblWithoutParams = tbl.filterNot { case (nm, _) => params.map(_.name).contains(nm) }
      val emptyParams: Chain[Param[NameWithPos]] = Chain.empty
      val emptyRes: Either[List[ResolverError], (Chain[Param[NameWithPos]], SymbolTable)] = Right((emptyParams, tblWithoutParams))

      val resolvedParams = params.foldLeft(emptyRes) {
        case (resSoFar, param @ Param(name, pos)) =>
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
        r1 <- resolvedParams
        (parms, updatedTbl) = r1
        r2 <- resolve(body, updatedTbl)
        (b, _) = r2
      } yield (Lambda(parms.toList, b, NameWithPos(NoName, pos)), tbl)


    case Apply(fn, args, pos) =>
      for {
        rf <- resolve(fn, tbl)
        (f, _) = rf

        ra <- args.foldLeft(EmptyResult) {
          case (resSoFar, nextArg) =>
            for {
              rs <- resSoFar
              a <- resolve(nextArg, tbl)
              (r, _) = a
            } yield rs :+ r
        }

      } yield (Apply(f, ra.toList, NameWithPos(NoName, pos)), tbl)
  }

  def resolve(mod: Module[Pos], decl: TopLevelDeclaration[Pos], tbl: SymbolTable): Either[List[ResolverError], (TopLevelDeclaration[NameWithPos], SymbolTable)] = decl match {
    case Let(name, expr, pos) =>
      val memberName = MemberName(mod.pkg, mod.name, name)

      resolve(expr, tbl).flatMap {
        case (resolvedExpr, updatedTbl) =>
          if (updatedTbl.contains(name))
            ResolverError.singleton(resolvedExpr.meta.pos, s"Symbol $name is already defined")
          else
            Right((Let(name, resolvedExpr, NameWithPos(memberName, pos)), updatedTbl.updated(name, memberName)))
      }
  }

  def resolve(
    module: Module[Pos],
    importedDecls: Map[String, TopLevelDeclaration[NameWithType]] = Map.empty
  ): Either[List[ResolverError], Module[NameWithPos]] = module match {
    case Module(pkg, name, _, decls, pos) =>
      val initialTbl = importedDecls.mapValues(_.meta.name)

      val emptyDecls = Chain.empty[TopLevelDeclaration[NameWithPos]]
      val emptyRes: Either[List[ResolverError], (Chain[TopLevelDeclaration[NameWithPos]], SymbolTable)] = Right((emptyDecls, initialTbl))

      val resolvedDecls = decls.foldLeft(emptyRes) {
        case (resSoFar, nextDecl) =>
          for {
            r <- resSoFar
            (resolvedSoFar, tblSoFar) = r
            c <- resolve(module, nextDecl, tblSoFar)
            (resolvedDecl, updatedTbl) = c
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
