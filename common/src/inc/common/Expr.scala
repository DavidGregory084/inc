package inc.common

import cats.Functor
import cats.syntax.functor._
import java.lang.{ Exception, String }
import qq.droste._
import qq.droste.data.Fix
import scala.{ Boolean, Char, Double, Float, Int, Long, Product, Serializable, Some, Nothing }
import scala.Predef.=:=
import scala.collection.immutable.{ List, Map, Set }

sealed trait Expr[A, B] extends Product with Serializable {
  def meta: A

  // def replace(mapping: Map[Reference[A], Reference[A]])(implicit eqv: A =:= NamePosType): Expr[A] = this match {
  //   case int @ LiteralInt(_, _) => int
  //   case long @ LiteralLong(_, _) => long
  //   case flt @ LiteralFloat(_, _) => flt
  //   case dbl @ LiteralDouble(_, _) => dbl
  //   case bool @ LiteralBoolean(_, _) => bool
  //   case str @ LiteralString(_, _) => str
  //   case chr @ LiteralChar(_, _) => chr
  //   case unit @ LiteralUnit(_) => unit
  //   case ref @ Reference(_, _) =>
  //     mapping.getOrElse(
  //       ref.copy(meta = eqv(meta).withEmptyPos.asInstanceOf[A]),
  //       ref
  //     )
  //   case ifExpr @ If(cond, thenExpr, elseExpr, _) =>
  //     ifExpr.copy(
  //       cond = cond.replace(mapping),
  //       thenExpr = thenExpr.replace(mapping),
  //       elseExpr = elseExpr.replace(mapping))
  //   case lam @ Lambda(_, body, _) =>
  //     lam.copy(body = body.replace(mapping))
  //   case app @ Apply(fn, args, _) =>
  //     app.copy(
  //       fn = fn.replace(mapping),
  //       args = args.map(_.replace(mapping)))
  // }

  // def substitute(subst: Map[TypeVariable, Type])(implicit eqv: A =:= NamePosType): Expr[A] =
  //   this.map(a => eqv(a).substitute(subst).asInstanceOf[A])
}
object Expr {
  def literalInt[A](i: Int, meta: A): ExprF[A] = Fix(LiteralInt(i, meta))
  def literalLong[A](l: Long, meta: A): ExprF[A] = Fix(LiteralLong(l, meta))
  def literalFloat[A](f: Float, meta: A): ExprF[A] = Fix(LiteralFloat(f, meta))
  def literalDouble[A](d: Double, meta: A): ExprF[A] = Fix(LiteralDouble(d, meta))
  def literalBoolean[A](b: Boolean, meta: A): ExprF[A] = Fix(LiteralBoolean(b, meta))
  def literalChar[A](c: Char, meta: A): ExprF[A] = Fix(LiteralChar(c, meta))
  def literalString[A](s: String, meta: A): ExprF[A] = Fix(LiteralString(s, meta))
  def literalUnit[A](meta: A): ExprF[A] = Fix(LiteralUnit(meta)) 
  def reference[A](name: String, meta: A): ExprF[A] = Fix(Reference(name, meta))
  def ifExpr[A](cond: ExprF[A], thenExpr: ExprF[A], elseExpr: ExprF[A], meta: A): ExprF[A] =
    Fix(If(cond, thenExpr, elseExpr, meta))
  def lambda[A](params: List[Param[A]], body: ExprF[A], meta: A): ExprF[A] =
    Fix(Lambda(params, body, meta))
  def app[A](fn: ExprF[A], args: List[ExprF[A]], meta: A): ExprF[A] =
    Fix(Apply(fn, args, meta))
 
  def capturedVariables[A](expr: ExprF[A])(implicit eqv: A =:= NamePosType): Set[Reference[NamePosType, Nothing]] =
    scheme.cata(Algebra[Expr[A, ?], Set[Reference[NamePosType, Nothing]]] {
      case LiteralInt(_, _) => Set.empty
      case LiteralLong(_, _) => Set.empty
      case LiteralFloat(_, _) => Set.empty
      case LiteralDouble(_, _) => Set.empty
      case LiteralBoolean(_, _) => Set.empty
      case LiteralString(_, _) => Set.empty
      case LiteralChar(_, _) => Set.empty
      case LiteralUnit(_) => Set.empty
      case Reference(nm, meta) =>
        Set(Reference(nm, eqv(meta)))
      case If(cond, thenExpr, elseExpr, _) =>
        cond ++ thenExpr ++ elseExpr
      case Lambda(params, body, _) =>
        val lambdaParams = params.map(p => Reference(p.name, eqv(p.meta).withEmptyPos))
        body -- lambdaParams
      case Apply(fn, args, _) =>
        fn ++ args.flatMap(x => x)
    }).apply(expr)

  def toProto[A](expr: ExprF[A])(implicit eqv: A =:= NamePosType): proto.Expr =
    scheme.cata(Algebra[Expr[A, ?], proto.Expr] {
      case LiteralInt(i, meta) =>
        val nameWithType = Some(eqv(meta).toProto)
        proto.LiteralInt(i, nameWithType)
      case LiteralLong(l, meta) =>
        val nameWithType = Some(eqv(meta).toProto)
        proto.LiteralLong(l, nameWithType)
      case LiteralFloat(f, meta) =>
        val nameWithType = Some(eqv(meta).toProto)
        proto.LiteralFloat(f, nameWithType)
      case LiteralDouble(d, meta) =>
        val nameWithType = Some(eqv(meta).toProto)
        proto.LiteralDouble(d, nameWithType)
      case LiteralBoolean(b, meta) =>
        val nameWithType = Some(eqv(meta).toProto)
        proto.LiteralBoolean(b, nameWithType)
      case LiteralString(s, meta) =>
        val nameWithType = Some(eqv(meta).toProto)
        proto.LiteralString(s, nameWithType)
      case LiteralChar(c, meta) =>
        val nameWithType = Some(eqv(meta).toProto)
        proto.LiteralChar(c.toString, nameWithType)
      case LiteralUnit(meta) =>
        val nameWithType = Some(eqv(meta).toProto)
        proto.LiteralUnit(nameWithType)
      case Reference(name, meta) =>
        val nameWithType = Some(eqv(meta).toProto)
        proto.Reference(name, nameWithType)
      case If(cond, thenExpr, elseExpr, meta) =>
        val nameWithType = Some(eqv(meta).toProto)
        proto.If(cond, thenExpr, elseExpr, nameWithType)
      case Lambda(params, body, meta) =>
        val nameWithType = Some(eqv(meta).toProto)
        proto.Lambda(params.map(_.toProto), body, nameWithType)
      case Apply(fn, args, meta) =>
        val nameWithType = Some(eqv(meta).toProto)
        proto.Apply(fn, args, nameWithType)
    }).apply(expr)

  def fromProto(expr: proto.Expr): ExprF[NameWithType] = expr match {
    case int @ proto.LiteralInt(i, _) =>
      Expr.literalInt(i, NameWithType.fromProto(int.getNameWithType))
    case long @ proto.LiteralLong(l, _) =>
      Expr.literalLong(l, NameWithType.fromProto(long.getNameWithType))
    case flt @ proto.LiteralFloat(f, _) =>
      Expr.literalFloat(f, NameWithType.fromProto(flt.getNameWithType))
    case dbl @ proto.LiteralDouble(d, _) =>
      Expr.literalDouble(d, NameWithType.fromProto(dbl.getNameWithType))
    case bool @ proto.LiteralBoolean(b, _) =>
      Expr.literalBoolean(b, NameWithType.fromProto(bool.getNameWithType))
    case str @ proto.LiteralString(s, _) =>
      Expr.literalString(s, NameWithType.fromProto(str.getNameWithType))
    case char @ proto.LiteralChar(c, _) =>
      Expr.literalChar(c.charAt(0), NameWithType.fromProto(char.getNameWithType))
    case unit @ proto.LiteralUnit(_) =>
      Expr.literalUnit(NameWithType.fromProto(unit.getNameWithType))
    case ref @ proto.Reference(name, _) =>
      Expr.reference(name, NameWithType.fromProto(ref.getNameWithType))
    case ifExpr @ proto.If(cond, thenExpr, elseExpr, _) =>
      Expr.ifExpr(
        Expr.fromProto(cond),
        Expr.fromProto(thenExpr),
        Expr.fromProto(elseExpr),
        NameWithType.fromProto(ifExpr.getNameWithType))
    case lambda @ proto.Lambda(params, body, _) =>
      Expr.lambda(
        params.map(Param.fromProto).toList,
        Expr.fromProto(body),
        NameWithType.fromProto(lambda.getNameWithType))
    case app @ proto.Apply(fn, args, _) =>
      Expr.app(
        Expr.fromProto(fn),
        args.toList.map(Expr.fromProto),
        NameWithType.fromProto(app.getNameWithType))
    case proto.Expr.Empty =>
      throw new Exception("Empty Expr in protobuf")
  }

  implicit def exprFunctor[A]: Functor[Expr[A, ?]] = new Functor[Expr[A, ?]] {
    def map[B, C](ea: Expr[A, B])(f: B => C): Expr[A, C] = ea match {
      case int @ LiteralInt(_, _) => int.asInstanceOf[Expr[A, C]]
      case long @ LiteralLong(_, _) => long.asInstanceOf[Expr[A, C]]
      case float @ LiteralFloat(_, _) => float.asInstanceOf[Expr[A, C]]
      case double @ LiteralDouble(_, _) => double.asInstanceOf[Expr[A, C]]
      case boolean @ LiteralBoolean(_, _) => boolean.asInstanceOf[Expr[A, C]]
      case string @ LiteralString(_, _) => string.asInstanceOf[Expr[A, C]]
      case char @ LiteralChar(_, _) => char.asInstanceOf[Expr[A, C]]
      case unit @ LiteralUnit(_) => unit.asInstanceOf[Expr[A, C]]
      case ref @ Reference(_, _) => ref.asInstanceOf[Expr[A, C]]
      case ifExpr @ If(_, _, _, _) =>
        ifExpr.copy(
          cond = f(ifExpr.cond),
          thenExpr = f(ifExpr.thenExpr),
          elseExpr = f(ifExpr.elseExpr),
          meta = ifExpr.meta)
      case lambda @ Lambda(_, _, _) =>
        lambda.copy(
          params = lambda.params,
          body = f(lambda.body),
          meta = lambda.meta)
      case app @ Apply(_, _, _) =>
        app.copy(
          fn = f(app.fn),
          args = app.args.map(f),
          meta = app.meta)
    }
  }
}

final case class If[A, B](
  cond: B,
  thenExpr: B,
  elseExpr: B,
  meta: A
) extends Expr[A, B]

final case class Lambda[A, B](
  params: List[Param[A]],
  body: B,
  meta: A
) extends Expr[A, B]

final case class Apply[A, B](
  fn: B,
  args: List[B],
  meta: A
) extends Expr[A, B]

final case class LiteralInt[A, B](i: Int, meta: A) extends Expr[A, B]
final case class LiteralLong[A, B](l: Long, meta: A) extends Expr[A, B]
final case class LiteralFloat[A, B](f: Float, meta: A) extends Expr[A, B]
final case class LiteralDouble[A, B](d: Double, meta: A) extends Expr[A, B]
final case class LiteralBoolean[A, B](b: Boolean, meta: A) extends Expr[A, B]
final case class LiteralChar[A, B](c: Char, meta: A) extends Expr[A, B]
final case class LiteralString[A, B](s: String, meta: A) extends Expr[A, B]
final case class LiteralUnit[A, B](meta: A) extends Expr[A, B]

final case class Reference[A, B](name: String, meta: A) extends Expr[A, B]
