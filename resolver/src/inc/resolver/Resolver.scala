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
        (c, t1) = r1
        r2 <- resolve(thenExpr, t1)
        (t, t2) = r2
        r3 <- resolve(elseExpr, t2)
        (e, t3) = r3
      } yield (If(c, t, e, NoName), t3)
    case Lambda(variable, body, _) =>
      for {
        r <- resolve(body, tbl)
        (b, t1) = r
      } yield (Lambda(variable, b, NoName), t1 + (variable -> LocalName(variable)))
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

  def resolve(
    module: Module[Unit],
    importedMods: Map[(List[String], String), Module[NameWithType]] = Map.empty
  ): Either[List[ResolverError], Module[Name]] = module match {
    case Module(pkg, name, imports, decls, _) =>
      val initialTbl = imports.foldLeft(Map.empty[String, Name]) {
        case (tbl, imprt) =>
          val (pkg, nm, syms) = imprt match {
            case ImportModule(pkg, nm) =>
              (pkg, nm, List.empty[String])
            case ImportSymbols(pkg, nm, syms) =>
              (pkg, nm, syms)
          }

          val updatedTbl = importedMods.get((pkg, nm)).map { mod =>
            val decls =
              if (syms.isEmpty)
                mod.declarations
              else
                mod.declarations.filter(d => syms.contains(d.name))

            decls.foldLeft(tbl) {
              case (tb, dcl) =>
                tb.updated(dcl.name, dcl.meta.name)
            }
          }

          updatedTbl.getOrElse(tbl)
      }

      val emptyDecls = Chain.empty[TopLevelDeclaration[Name]]
      val emptyRes: Either[List[ResolverError], (Chain[TopLevelDeclaration[Name]], SymbolTable)] = Right((emptyDecls, initialTbl))

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
          module.copy(declarations = resolved.toList, meta = ModuleName(pkg, name))
      }
  }
}
