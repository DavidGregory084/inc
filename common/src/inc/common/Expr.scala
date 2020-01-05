package inc.common

import cats.Functor
import cats.syntax.functor._
import java.lang.{ Exception, String }
import scala.{ =:=, Boolean, Char, Double, Float, Int, Long, Product, Serializable, Some, StringContext }
import scala.collection.immutable.{ List, Map, Set }
import scala.Predef.wrapRefArray

sealed abstract class Expr[A] extends Product with Serializable {
  def meta: A

  def capturedVariables(implicit eqv: A =:= Meta.Typed): Set[Reference[Meta.Typed]] = this match {
    case LiteralInt(_, _) => Set.empty
    case LiteralLong(_, _) => Set.empty
    case LiteralFloat(_, _) => Set.empty
    case LiteralDouble(_, _) => Set.empty
    case LiteralBoolean(_, _) => Set.empty
    case LiteralString(_, _) => Set.empty
    case LiteralChar(_, _) => Set.empty
    case LiteralUnit(_) => Set.empty
    case Reference(mod, nm, meta) =>
      Set(Reference(mod, nm, eqv(meta)))
    case If(cond, thenExpr, elseExpr, _) =>
      cond.capturedVariables ++ thenExpr.capturedVariables ++ elseExpr.capturedVariables
    case Lambda(params, body, _) =>
      val lambdaParams = params.map(p => Reference(List.empty, p.name, eqv(p.meta).forgetPos))
      val capturedInBody = body.capturedVariables.map(v => v.copy(meta = v.meta.forgetPos))
      capturedInBody -- lambdaParams
    case Apply(fn, args, _) =>
      fn.capturedVariables ++ args.flatMap(_.capturedVariables)
    case Ascription(expr, _, _) =>
      expr.capturedVariables
  }

  def replace(mapping: Map[Reference[A], Reference[A]])(implicit to: A =:= Meta.Typed): Expr[A] = this match {
    case int @ LiteralInt(_, _) => int
    case long @ LiteralLong(_, _) => long
    case flt @ LiteralFloat(_, _) => flt
    case dbl @ LiteralDouble(_, _) => dbl
    case bool @ LiteralBoolean(_, _) => bool
    case str @ LiteralString(_, _) => str
    case chr @ LiteralChar(_, _) => chr
    case unit @ LiteralUnit(_) => unit
    case ref @ Reference(_, _, _) =>
      val from = to.flip
      mapping.getOrElse(
        ref.copy(meta = from(to(meta).forgetPos)),
        ref
      )
    case ifExpr @ If(cond, thenExpr, elseExpr, _) =>
      ifExpr.copy(
        cond = cond.replace(mapping),
        thenExpr = thenExpr.replace(mapping),
        elseExpr = elseExpr.replace(mapping))
    case lam @ Lambda(_, body, _) =>
      lam.copy(body = body.replace(mapping))
    case app @ Apply(fn, args, _) =>
      app.copy(
        fn = fn.replace(mapping),
        args = args.map(_.replace(mapping)))
    case asc @ Ascription(expr, _, _) =>
      asc.copy(expr = expr.replace(mapping))
  }

  def toProto(implicit eqv: A =:= Meta.Typed): proto.Expr = this match {
    case LiteralInt(i, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.LiteralInt(i, typedMeta)
    case LiteralLong(l, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.LiteralLong(l, typedMeta)
    case LiteralFloat(f, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.LiteralFloat(f, typedMeta)
    case LiteralDouble(d, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.LiteralDouble(d, typedMeta)
    case LiteralBoolean(b, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.LiteralBoolean(b, typedMeta)
    case LiteralString(s, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.LiteralString(s, typedMeta)
    case LiteralChar(c, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.LiteralChar(c.toString, typedMeta)
    case LiteralUnit(meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.LiteralUnit(typedMeta)
    case Reference(mod, name, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      val modString = mod.mkString("/")
      val fullName = if (mod.isEmpty) name else s"${modString}.${name}"
      proto.Reference(fullName, typedMeta)
    case If(cond, thenExpr, elseExpr, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.If(cond.toProto, thenExpr.toProto, elseExpr.toProto, typedMeta)
    case Lambda(params, body, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.Lambda(params.map(_.toProto), body.toProto, typedMeta)
    case Apply(fn, args, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.Apply(fn.toProto, args.map(_.toProto), typedMeta)
    case Ascription(expr, ascribedAs, meta) =>
      val typedMeta = Some(eqv(meta).toProto)
      proto.Ascription(expr.toProto, Some(ascribedAs.toProto), typedMeta)
  }

  def substitute(subst: Map[TypeVariable, Type])(implicit to: A =:= Meta.Typed): Expr[A] = {
    if (subst.isEmpty)
      this
    else {
      val from = to.flip
      this.map(a => from(to(a).substitute(subst)))
    }
  }
}
object Expr {
  def fromProto(expr: proto.Expr): Expr[Meta.Typed] = expr match {
    case int @ proto.LiteralInt(i, _) =>
      LiteralInt(i, Meta.fromProto(int.getNameWithType))
    case long @ proto.LiteralLong(l, _) =>
      LiteralLong(l, Meta.fromProto(long.getNameWithType))
    case flt @ proto.LiteralFloat(f, _) =>
      LiteralFloat(f, Meta.fromProto(flt.getNameWithType))
    case dbl @ proto.LiteralDouble(d, _) =>
      LiteralDouble(d, Meta.fromProto(dbl.getNameWithType))
    case bool @ proto.LiteralBoolean(b, _) =>
      LiteralBoolean(b, Meta.fromProto(bool.getNameWithType))
    case str @ proto.LiteralString(s, _) =>
      LiteralString(s, Meta.fromProto(str.getNameWithType))
    case char @ proto.LiteralChar(c, _) =>
      LiteralChar(c.charAt(0), Meta.fromProto(char.getNameWithType))
    case unit @ proto.LiteralUnit(_) =>
      LiteralUnit(Meta.fromProto(unit.getNameWithType))
    case ref @ proto.Reference(nm, _) =>
      val mod = nm.split("/").toList
      val name = mod.last.split("\\.").last
      Reference(mod.dropRight(1), name, Meta.fromProto(ref.getNameWithType))
    case ifExpr @ proto.If(cond, thenExpr, elseExpr, _) =>
      If(
        Expr.fromProto(cond),
        Expr.fromProto(thenExpr),
        Expr.fromProto(elseExpr),
        Meta.fromProto(ifExpr.getNameWithType))
    case lambda @ proto.Lambda(params, body, _) =>
      Lambda(
        params.map(Param.fromProto).toList,
        Expr.fromProto(body),
        Meta.fromProto(lambda.getNameWithType))
    case app @ proto.Apply(fn, args, _) =>
      Apply(
        Expr.fromProto(fn),
        args.toList.map(Expr.fromProto),
        Meta.fromProto(app.getNameWithType))
    case asc @ proto.Ascription(_, _, _) =>
      Ascription(
        Expr.fromProto(asc.expr),
        TypeScheme.fromProto(asc.getAscribedAs),
        Meta.fromProto(asc.getNameWithType))
    case proto.Expr.Empty =>
      throw new Exception("Empty Expr in protobuf")
  }

  implicit val exprFunctor: Functor[Expr] = new Functor[Expr] {
    def map[A, B](ea: Expr[A])(f: A => B): Expr[B] = ea match {
      case int @ LiteralInt(_, _) =>
        int.copy(meta = f(int.meta))
      case long @ LiteralLong(_, _) =>
        long.copy(meta = f(long.meta))
      case float @ LiteralFloat(_, _) =>
        float.copy(meta = f(float.meta))
      case double @ LiteralDouble(_, _) =>
        double.copy(meta = f(double.meta))
      case boolean @ LiteralBoolean(_, _) =>
        boolean.copy(meta = f(boolean.meta))
      case string @ LiteralString(_, _) =>
        string.copy(meta = f(string.meta))
      case char @ LiteralChar(_, _) =>
        char.copy(meta = f(char.meta))
      case unit @ LiteralUnit(_) =>
        unit.copy(meta = f(unit.meta))
      case ref @ Reference(_, _, _) =>
        ref.copy(meta = f(ref.meta))
      case ifExpr @ If(_, _, _, _) =>
        ifExpr.copy(
          cond = map(ifExpr.cond)(f),
          thenExpr = map(ifExpr.thenExpr)(f),
          elseExpr = map(ifExpr.elseExpr)(f),
          meta = f(ifExpr.meta))
      case lambda @ Lambda(_, _, _) =>
        lambda.copy(
          params = lambda.params.map(_.map(f)),
          body = map(lambda.body)(f),
          meta = f(lambda.meta))
      case app @ Apply(_, _, _) =>
        app.copy(
          fn = map(app.fn)(f),
          args = app.args.map(map(_)(f)),
          meta = f(app.meta))
      case asc @ Ascription(_, _, _) =>
        asc.copy(
          expr = map(asc.expr)(f),
          meta = f(asc.meta))
    }
  }
}

final case class If[A](
  cond: Expr[A],
  thenExpr: Expr[A],
  elseExpr: Expr[A],
  meta: A
) extends Expr[A]

final case class Lambda[A](
  params: List[Param[A]],
  body: Expr[A],
  meta: A
) extends Expr[A]

final case class Apply[A](
  fn: Expr[A],
  args: List[Expr[A]],
  meta: A
) extends Expr[A]

final case class Ascription[A](
  expr: Expr[A],
  ascribedAs: TypeScheme,
  meta: A
) extends Expr[A]

final case class LiteralInt[A](i: Int, meta: A) extends Expr[A]
final case class LiteralLong[A](l: Long, meta: A) extends Expr[A]
final case class LiteralFloat[A](f: Float, meta: A) extends Expr[A]
final case class LiteralDouble[A](d: Double, meta: A) extends Expr[A]
final case class LiteralBoolean[A](b: Boolean, meta: A) extends Expr[A]
final case class LiteralChar[A](c: Char, meta: A) extends Expr[A]
final case class LiteralString[A](s: String, meta: A) extends Expr[A]
final case class LiteralUnit[A](meta: A) extends Expr[A]

final case class Reference[A](mod: List[String], name: String, meta: A) extends Expr[A] {
  def fullName = if (mod.isEmpty) name else mod.mkString("/") + "." + name
}
