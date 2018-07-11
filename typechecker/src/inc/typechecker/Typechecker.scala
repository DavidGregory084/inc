package inc.typechecker

import inc.common._

object Typechecker {
  case class Environment(env: Map[String, Type])

  def typecheck(expr: Expr[Unit], env: Environment): (Either[List[TypeError], Expr[Type]], Environment) = expr match {
    case int @ LiteralInt(_, _) =>
      (Right(int.copy(meta = Type.Int)), env)
    case long @ LiteralLong(_, _) =>
      (Right(long.copy(meta = Type.Long)), env)
    case float @ LiteralFloat(_, _) =>
      (Right(float .copy(meta = Type.Float)), env)
    case double @ LiteralDouble(_, _) =>
      (Right(double.copy(meta = Type.Double)), env)
    case bool @ LiteralBoolean(_, _) =>
      (Right(bool.copy(meta = Type.Boolean)), env)
    case char @ LiteralChar(_, _) =>
      (Right(char.copy(meta = Type.Char)), env)
    case str @ LiteralString(_, _) =>
      (Right(str.copy(meta = Type.String)), env)
    case ref @ Reference(name, _)  =>
      (env.env.get(name).map { typ =>
        Right(ref.copy(meta = typ))
      }.getOrElse(Left(List(TypeError(s"Reference to undefined symbol: $name")))), env)
  }

  def typecheck(decl: TopLevelDeclaration[Unit], env: Environment): (Either[List[TypeError], TopLevelDeclaration[Type]], Environment) = decl match {
    case Let(name, expr, _) =>
      val (res, updatedEnv) = typecheck(expr, env)
      val updatedWithThis = res.toOption.fold(updatedEnv.env)(e => updatedEnv.env.updated(name, e.meta))
      (res.map(e => Let(name, e, e.meta)), Environment(updatedWithThis))
  }

  def typecheck(module: Module[Unit]): Either[List[TypeError], Module[Type]] = module match {
    case Module(_, _, decls, _) =>
      val emptyEnv = Environment(Map.empty)

      val typecheckedDecls = decls.foldLeft((Seq.empty[Either[List[TypeError], TopLevelDeclaration[Type]]], emptyEnv)) {
        case ((checked, env), decl) =>
          val (res, updatedEnv) = typecheck(decl, env)
          (checked :+ res, updatedEnv)
      }

      typecheckedDecls match {
        case (checked, _) =>
          if (checked.forall(_.isRight))
            Right(
              module.copy(
                declarations = checked.map(_.right.get),
                meta = Type.Module))
          else
            Left(
              checked.toList.filter(_.isLeft).flatMap(_.left.get))
      }
  }
}
