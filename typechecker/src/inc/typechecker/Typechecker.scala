package inc.typechecker

import cats.data.Chain
import inc.common._

object Typechecker {
  type Environment = Map[String, Type]

  def typecheck(expr: Expr[Name], env: Environment): Either[List[TypeError], (Expr[NameWithType], Environment)] = expr match {
    case int @ LiteralInt(_, _) =>
      Right((int.copy(meta = NameWithType(int.meta, Type.Int)), env))
    case long @ LiteralLong(_, _) =>
      Right((long.copy(meta = NameWithType(long.meta, Type.Long)), env))
    case float @ LiteralFloat(_, _) =>
      Right((float.copy(meta = NameWithType(float.meta, Type.Float)), env))
    case double @ LiteralDouble(_, _) =>
      Right((double.copy(meta = NameWithType(double.meta, Type.Double)), env))
    case bool @ LiteralBoolean(_, _) =>
      Right((bool.copy(meta = NameWithType(bool.meta, Type.Boolean)), env))
    case char @ LiteralChar(_, _) =>
      Right((char.copy(meta = NameWithType(char.meta, Type.Char)), env))
    case str @ LiteralString(_, _) =>
      Right((str.copy(meta = NameWithType(str.meta, Type.String)), env))
    case unit @ LiteralUnit(_) =>
      Right((unit.copy(meta = NameWithType(unit.meta, Type.Unit)), env))
    case ref @ Reference(name, _)  =>
      env.get(name).map { typ =>
        Right((ref.copy(meta = NameWithType(ref.meta, typ)), env))
      }.getOrElse(TypeError.singleton(s"Reference to undefined symbol: $name"))
    case If(_, _, _, _) =>
      ???
  }

  def typecheck(decl: TopLevelDeclaration[Name], env: Environment): Either[List[TypeError], (TopLevelDeclaration[NameWithType], Environment)] = decl match {
    case Let(name, expr, _) =>
      typecheck(expr, env).flatMap {
        case (checkedExpr, updatedEnv) =>
          if (updatedEnv.contains(name))
            TypeError.singleton(s"Symbol $name is already defined")
          else
            Right((Let(name, checkedExpr, NameWithType(decl.meta, checkedExpr.meta.typ)), updatedEnv.updated(name, checkedExpr.meta.typ)))
      }
  }

  def typecheck(
    module: Module[Name],
    importedMods: Map[(List[String], String), Module[NameWithType]] = Map.empty
  ): Either[List[TypeError], Module[NameWithType]] = module match {
    case Module(_, _, imports, decls, _) =>
      val initialEnv = imports.foldLeft(Map.empty[String, Type]) {
        case (env, imprt) =>
          val (pkg, nm, syms) = imprt match {
            case ImportModule(pkg, nm) =>
              (pkg, nm, List.empty[String])
            case ImportSymbols(pkg, nm, syms) =>
              (pkg, nm, syms)
          }

          val updatedEnv = importedMods.get((pkg, nm)).map { mod =>
            val decls =
              if (syms.isEmpty)
                mod.declarations
              else
                mod.declarations.filter(d => syms.contains(d.name))

            decls.foldLeft(env) {
              case (e, dcl) =>
                e.updated(dcl.name, dcl.meta.typ)
            }
          }

          updatedEnv.getOrElse(env)
      }

      val emptyDecls = Chain.empty[TopLevelDeclaration[NameWithType]]
      val emptyRes: Either[List[TypeError], (Chain[TopLevelDeclaration[NameWithType]], Environment)] = Right((emptyDecls, initialEnv))

      val typecheckedDecls = decls.foldLeft(emptyRes) {
        case (resSoFar, nextDecl) =>
          for {
            r <- resSoFar
            (checkedSoFar, envSoFar) = r
            c <- typecheck(nextDecl, envSoFar)
            (checkedDecl, updatedEnv) = c
          } yield (checkedSoFar :+ checkedDecl, updatedEnv)
      }

      typecheckedDecls.map {
        case (checked, _) =>
          module.copy(declarations = checked.toList, meta = NameWithType(module.meta, Type.Module))
      }
  }
}
