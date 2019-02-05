package inc.common

import cats.Functor
import cats.syntax.functor._
import java.lang.{ Exception, String }
import scala.{ Boolean, Char, Double, Float, Int, Long, Some }
import scala.Predef.=:=
import scala.collection.immutable.{ List, Map, Set }

sealed abstract class Expr[A] extends Tree {
  def meta: A

  def capturedVariables(implicit eqv: A =:= NameWithType): Set[Reference[NameWithType]] = this match {
    case LiteralInt(_, _, _) => Set.empty
    case LiteralLong(_, _, _) => Set.empty
    case LiteralFloat(_, _, _) => Set.empty
    case LiteralDouble(_, _, _) => Set.empty
    case LiteralBoolean(_, _, _) => Set.empty
    case LiteralString(_, _, _) => Set.empty
    case LiteralChar(_, _, _) => Set.empty
    case LiteralUnit(_, _) => Set.empty
    case Reference(nm, pos, meta) =>
      Set(Reference(nm, pos, eqv(meta)))
    case If(cond, thenExpr, elseExpr, _, _) =>
      cond.capturedVariables ++ thenExpr.capturedVariables ++ elseExpr.capturedVariables
    case Lambda(params, body, _, _) =>
      val lambdaParams = params.map(p => Reference(p.name, Pos.Empty, eqv(p.meta)))
      val capturedInBody = body.capturedVariables.map(v => v.copy(pos = Pos.Empty))
      capturedInBody -- lambdaParams
    case Apply(fn, args, _, _) =>
      fn.capturedVariables ++ args.flatMap(_.capturedVariables)
  }

  def replace(mapping: Map[Reference[A], Reference[A]])(implicit eqv: A =:= NameWithType): Expr[A] = this match {
    case int @ LiteralInt(_, _, _) => int
    case long @ LiteralLong(_, _, _) => long
    case flt @ LiteralFloat(_, _, _) => flt
    case dbl @ LiteralDouble(_, _, _) => dbl
    case bool @ LiteralBoolean(_, _, _) => bool
    case str @ LiteralString(_, _, _) => str
    case chr @ LiteralChar(_, _, _) => chr
    case unit @ LiteralUnit(_, _) => unit
    case ref @ Reference(_, _, _) =>
      val remap = mapping.getOrElse(
        ref.copy(pos = Pos.Empty, meta = eqv(meta).asInstanceOf[A]),
        ref
      )
      scala.Predef.println("From: " + ref + " To: " + remap)
      remap
    case ifExpr @ If(cond, thenExpr, elseExpr, _, _) =>
      ifExpr.copy(
        cond = cond.replace(mapping),
        thenExpr = thenExpr.replace(mapping),
        elseExpr = elseExpr.replace(mapping))
    case lam @ Lambda(_, body, _, _) =>
      lam.copy(body = body.replace(mapping))
    case app @ Apply(fn, args, _, _) =>
      app.copy(
        fn = fn.replace(mapping),
        args = args.map(_.replace(mapping)))
  }

  def toProto(implicit eqv: A =:= NameWithType): proto.Expr = this match {
    case LiteralInt(i, _, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralInt(i, nameWithType)
    case LiteralLong(l, _, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralLong(l, nameWithType)
    case LiteralFloat(f, _, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralFloat(f, nameWithType)
    case LiteralDouble(d, _, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralDouble(d, nameWithType)
    case LiteralBoolean(b, _, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralBoolean(b, nameWithType)
    case LiteralString(s, _, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralString(s, nameWithType)
    case LiteralChar(c, _, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralChar(c.toString, nameWithType)
    case LiteralUnit(_, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralUnit(nameWithType)
    case Reference(name, _, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Reference(name, nameWithType)
    case If(cond, thenExpr, elseExpr, _, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.If(cond.toProto, thenExpr.toProto, elseExpr.toProto, nameWithType)
    case Lambda(params, body, _, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Lambda(params.map(_.toProto), body.toProto, nameWithType)
    case Apply(fn, args, _, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Apply(fn.toProto, args.map(_.toProto), nameWithType)
  }

  def substitute(subst: Map[TypeVariable, Type])(implicit eqv: A =:= NameWithType): Expr[A] =
    this.map(a => eqv(a).substitute(subst).asInstanceOf[A])
}
object Expr {
  def fromProto(expr: proto.Expr): Expr[NameWithType] = expr match {
    case int @ proto.LiteralInt(i, _) =>
      LiteralInt(i, Pos.Empty, NameWithType.fromProto(int.getNameWithType))
    case long @ proto.LiteralLong(l, _) =>
      LiteralLong(l, Pos.Empty, NameWithType.fromProto(long.getNameWithType))
    case flt @ proto.LiteralFloat(f, _) =>
      LiteralFloat(f, Pos.Empty, NameWithType.fromProto(flt.getNameWithType))
    case dbl @ proto.LiteralDouble(d, _) =>
      LiteralDouble(d, Pos.Empty, NameWithType.fromProto(dbl.getNameWithType))
    case bool @ proto.LiteralBoolean(b, _) =>
      LiteralBoolean(b, Pos.Empty, NameWithType.fromProto(bool.getNameWithType))
    case str @ proto.LiteralString(s, _) =>
      LiteralString(s, Pos.Empty, NameWithType.fromProto(str.getNameWithType))
    case char @ proto.LiteralChar(c, _) =>
      LiteralChar(c.charAt(0), Pos.Empty, NameWithType.fromProto(char.getNameWithType))
    case unit @ proto.LiteralUnit(_) =>
      LiteralUnit(Pos.Empty, NameWithType.fromProto(unit.getNameWithType))
    case ref @ proto.Reference(name, _) =>
      Reference(name, Pos.Empty, NameWithType.fromProto(ref.getNameWithType))
    case ifExpr @ proto.If(cond, thenExpr, elseExpr, _) =>
      If(
        Expr.fromProto(cond),
        Expr.fromProto(thenExpr),
        Expr.fromProto(elseExpr),
        Pos.Empty,
        NameWithType.fromProto(ifExpr.getNameWithType))
    case lambda @ proto.Lambda(params, body, _) =>
      Lambda(
        params.map(Param.fromProto).toList,
        Expr.fromProto(body),
        Pos.Empty,
        NameWithType.fromProto(lambda.getNameWithType))
    case app @ proto.Apply(fn, args, _) =>
      Apply(
        Expr.fromProto(fn),
        args.toList.map(Expr.fromProto),
        Pos.Empty,
        NameWithType.fromProto(app.getNameWithType))
    case proto.Expr.Empty =>
      throw new Exception("Empty Expr in protobuf")
  }

  implicit val exprFunctor: Functor[Expr] = new Functor[Expr] {
    def map[A, B](ea: Expr[A])(f: A => B): Expr[B] = ea match {
      case int @ LiteralInt(_, _, _) =>
        int.copy(meta = f(int.meta))
      case long @ LiteralLong(_, _, _) =>
        long.copy(meta = f(long.meta))
      case float @ LiteralFloat(_, _, _) =>
        float.copy(meta = f(float.meta))
      case double @ LiteralDouble(_, _, _) =>
        double.copy(meta = f(double.meta))
      case boolean @ LiteralBoolean(_, _, _) =>
        boolean.copy(meta = f(boolean.meta))
      case string @ LiteralString(_, _, _) =>
        string.copy(meta = f(string.meta))
      case char @ LiteralChar(_, _, _) =>
        char.copy(meta = f(char.meta))
      case unit @ LiteralUnit(_, _) =>
        unit.copy(meta = f(unit.meta))
      case ref @ Reference(_, _, _) =>
        ref.copy(meta = f(ref.meta))
      case ifExpr @ If(_, _, _, _, _) =>
        ifExpr.copy(
          cond = map(ifExpr.cond)(f),
          thenExpr = map(ifExpr.thenExpr)(f),
          elseExpr = map(ifExpr.elseExpr)(f),
          meta = f(ifExpr.meta))
      case lambda @ Lambda(_, _, _, _) =>
        lambda.copy(
          params = lambda.params.map(_.map(f)),
          body = map(lambda.body)(f),
          meta = f(lambda.meta))
      case app @ Apply(_, _, _, _) =>
        app.copy(
          fn = map(app.fn)(f),
          args = app.args.map(map(_)(f)),
          meta = f(app.meta))
    }
  }
}

final case class If[A](
  cond: Expr[A],
  thenExpr: Expr[A],
  elseExpr: Expr[A],
  pos: Pos,
  meta: A
) extends Expr[A]

final case class Lambda[A](
  params: List[Param[A]],
  body: Expr[A],
  pos: Pos,
  meta: A
) extends Expr[A]

final case class Apply[A](
  fn: Expr[A],
  args: List[Expr[A]],
  pos: Pos,
  meta: A
) extends Expr[A]

final case class LiteralInt[A](i: Int, pos: Pos, meta: A) extends Expr[A]
final case class LiteralLong[A](l: Long, pos: Pos, meta: A) extends Expr[A]
final case class LiteralFloat[A](f: Float, pos: Pos, meta: A) extends Expr[A]
final case class LiteralDouble[A](d: Double, pos: Pos, meta: A) extends Expr[A]
final case class LiteralBoolean[A](b: Boolean, pos: Pos, meta: A) extends Expr[A]
final case class LiteralChar[A](c: Char, pos: Pos, meta: A) extends Expr[A]
final case class LiteralString[A](s: String, pos: Pos, meta: A) extends Expr[A]
final case class LiteralUnit[A](pos: Pos, meta: A) extends Expr[A]

final case class Reference[A](name: String, pos: Pos, meta: A) extends Expr[A]
