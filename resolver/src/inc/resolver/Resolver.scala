package inc.resolver

import cats.data.Chain
import inc.common._

object Resolver {
  type SymbolTable = Map[String, Name]

  def resolve(expr: Expr[Unit], tbl: SymbolTable): Either[List[ResolverError], (Expr[Name], SymbolTable)] = expr match {
    case int @ LiteralInt(_, _) =>
      Right((int.copy(meta = NoName), tbl))
    case long @ LiteralLong(_, _) =>
      Right((long.copy(meta = NoName), tbl))
    case float @ LiteralFloat(_, _) =>
      Right((float.copy(meta = NoName), tbl))
    case double @ LiteralDouble(_, _) =>
      Right((double.copy(meta = NoName), tbl))
    case bool @ LiteralBoolean(_, _) =>
      Right((bool.copy(meta = NoName), tbl))
    case char @ LiteralChar(_, _) =>
      Right((char.copy(meta = NoName), tbl))
    case str @ LiteralString(_, _) =>
      Right((str.copy(meta = NoName), tbl))
    case unit @ LiteralUnit(_) =>
      Right((unit.copy(meta = NoName), tbl))
    case ref @ Reference(name, _)  =>
      tbl.get(name).map { nm =>
        Right((ref.copy(meta = nm), tbl))
      }.getOrElse(ResolverError.singleton(s"Reference to undefined symbol: $name"))
    case If(cond, thenExpr, elseExpr, _) =>
      for {
        r1 <- resolve(cond, tbl)
        (c, _) = r1
        r2 <- resolve(thenExpr, tbl)
        (t, _) = r2
        r3 <- resolve(elseExpr, tbl)
        (e, _) = r3
      } yield (If(c, t, e, NoName), tbl)
    case Lambda(variable, body, _) =>
      for {
        r <- resolve(body, tbl + (variable -> LocalName(variable)))
        (b, _) = r
      } yield (Lambda(variable, b, NoName), tbl)
    case Apply(fn, args, _) =>
      for {
        rf <- resolve(fn, tbl)
        (f, _) = rf

        emptyRes: Either[List[ResolverError], Chain[Expr[Name]]] = Right(Chain.empty)

        ra <- args.foldLeft(emptyRes) {
          case (resSoFar, nextArg) =>
            for {
              rs <- resSoFar
              a <- resolve(nextArg, tbl)
              (r, _) = a
            } yield rs :+ r
        }

      } yield (Apply(f, ra.toList, NoName), tbl)
  }

  def resolve(mod: Module[Unit], decl: TopLevelDeclaration[Unit], tbl: SymbolTable): Either[List[ResolverError], (TopLevelDeclaration[Name], SymbolTable)] = decl match {
    case Let(name, expr, _) =>
      val memberName = MemberName(mod.pkg, mod.name, name)

      resolve(expr, tbl).flatMap {
        case (resolvedExpr, updatedTbl) =>
          if (updatedTbl.contains(name))
            ResolverError.singleton(s"Symbol $name is already defined as ${updatedTbl(name)}")
          else
            Right((Let(name, resolvedExpr, memberName), updatedTbl.updated(name, memberName)))
      }
  }

  def resolve(
    module: Module[Unit],
    importedDecls: Map[String, TopLevelDeclaration[NameWithType]] = Map.empty
  ): Either[List[ResolverError], Module[Name]] = module match {
    case Module(pkg, name, _, decls, _) =>
      val initialTbl = importedDecls.mapValues(_.meta.name)

      val emptyDecls = Chain.empty[TopLevelDeclaration[Name]]
      val emptyRes: Either[List[ResolverError], (Chain[TopLevelDeclaration[Name]], SymbolTable)] = Right((emptyDecls, initialTbl))

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
          module.copy(declarations = resolved.toList, meta = ModuleName(pkg, name))
      }
  }
}
