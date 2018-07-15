package inc.typechecker

import inc.common._

object Typechecker {
  type Environment = Map[String, Type]

  def typecheck(expr: Expr[Unit], env: Environment): Either[List[TypeError], (Expr[Type], Environment)] = expr match {
    case int @ LiteralInt(_, _) =>
      Right((int.copy(meta = Type.Int), env))
    case long @ LiteralLong(_, _) =>
      Right((long.copy(meta = Type.Long), env))
    case float @ LiteralFloat(_, _) =>
      Right((float.copy(meta = Type.Float), env))
    case double @ LiteralDouble(_, _) =>
      Right((double.copy(meta = Type.Double), env))
    case bool @ LiteralBoolean(_, _) =>
      Right((bool.copy(meta = Type.Boolean), env))
    case char @ LiteralChar(_, _) =>
      Right((char.copy(meta = Type.Char), env))
    case str @ LiteralString(_, _) =>
      Right((str.copy(meta = Type.String), env))
    case ref @ Reference(name, _)  =>
      env.get(name).map { typ =>
        Right((ref.copy(meta = typ), env))
      }.getOrElse(TypeError.singleton(s"Reference to undefined symbol: $name"))
  }

  def typecheck(decl: TopLevelDeclaration[Unit], env: Environment): Either[List[TypeError], (TopLevelDeclaration[Type], Environment)] = decl match {
    case Let(name, expr, _) =>
      typecheck(expr, env).flatMap {
        case (checkedExpr, updatedEnv) =>
          if (updatedEnv.contains(name))
            TypeError.singleton(s"Symbol $name is already defined")
          else
            Right((Let(name, checkedExpr, checkedExpr.meta), updatedEnv.updated(name, checkedExpr.meta)))
      }
  }

  def typecheck(module: Module[Unit]): Either[List[TypeError], Module[Type]] = module match {
    case Module(_, _, decls, _) =>
      val emptyEnv = Map.empty[String, Type]
      val emptyDecls = Seq.empty[TopLevelDeclaration[Type]]
      val emptyRes: Either[List[TypeError], (Seq[TopLevelDeclaration[Type]], Environment)] = Right((emptyDecls, emptyEnv))

      val typecheckedDecls = decls.foldLeft(emptyRes) {
        case (resSoFar, nextDecl) =>
          for {
            r <- resSoFar
            (checkedSoFar, envSoFar) = r
            c <- typecheck(nextDecl, envSoFar)
            (checkedDecl, updatedEnv) = c
          } yield (checkedSoFar :+ checkedDecl, updatedEnv)
      }

      typecheckedDecls map {
        case (checked, _) =>
          module.copy(declarations = checked, meta = Type.Module)
      }
  }
}
