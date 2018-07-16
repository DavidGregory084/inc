package inc.resolver

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
    case ref @ Reference(name, _)  =>
      tbl.get(name).map { nm =>
        Right((ref.copy(meta = nm), tbl))
      }.getOrElse(ResolverError.singleton(s"Reference to undefined symbol: $name"))
  }

  def resolve(decl: TopLevelDeclaration[Unit], tbl: SymbolTable): Either[List[ResolverError], (TopLevelDeclaration[Name], SymbolTable)] = decl match {
    case Let(name, expr, _) =>
      val localName = LocalName(name)
      resolve(expr, tbl).flatMap {
        case (resolvedExpr, updatedTbl) =>
          if (updatedTbl.contains(name))
            ResolverError.singleton(s"Symbol $name is already defined as ${updatedTbl(name)}")
          else
            Right((Let(name, resolvedExpr, localName), updatedTbl.updated(name, localName)))
      }
  }

  def resolve(module: Module[Unit]): Either[List[ResolverError], Module[Name]] = module match {
    case Module(pkg, name, decls, _) =>
      val emptyTbl = Map.empty[String, Name]
      val emptyDecls = Seq.empty[TopLevelDeclaration[Name]]
      val emptyRes: Either[List[ResolverError], (Seq[TopLevelDeclaration[Name]], SymbolTable)] = Right((emptyDecls, emptyTbl))

      val resolvedDecls = decls.foldLeft(emptyRes) {
        case (resSoFar, nextDecl) =>
          for {
            r <- resSoFar
            (resolvedSoFar, tblSoFar) = r
            c <- resolve(nextDecl, tblSoFar)
            (resolvedDecl, updatedTbl) = c
          } yield (resolvedSoFar :+ resolvedDecl, updatedTbl)
      }

      resolvedDecls.map {
        case (resolved, _) =>
          module.copy(declarations = resolved, meta = FullName(pkg, name))
      }
  }
}
