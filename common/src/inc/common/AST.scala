package inc.common

import cats._
import cats.implicits._
import java.util.concurrent.atomic.AtomicInteger

abstract class Error(val pos: Pos, val msg: String) extends Throwable(msg) with Product with Serializable

case class Pos(from: Int, to: Int)
object Pos {
  def Empty = Pos(-1, -1)
}

sealed trait Tree[A] extends Product with Serializable {
  def meta: A
}

object Tree {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](ta: Tree[A])(f: A => B): Tree[B] = ta match {
      case mod: Module[_] => mod.map(f)
      case top: TopLevelDeclaration[_] => top.map(f)
      case exp: Expr[_] => exp.map(f)
    }
  }
}

final case class Module[A](
  pkg: List[String],
  name: String,
  imports: List[Import],
  declarations: List[TopLevelDeclaration[A]],
  meta: A,
) extends Tree[A] {
  def toProto(implicit eqv: A =:= NamePosType): proto.Module = proto.Module(
    pkg = pkg,
    name = name,
    imports = imports.map(_.toProto),
    declarations = declarations.map(_.toProto),
    nameWithType = Some(eqv(meta).toProto)
  )

  def substitute(subst: Map[TypeVariable, Type])(implicit eqv: A =:= NamePosType): Module[A] =
    this.map(a => eqv(a).substitute(subst).asInstanceOf[A])
}
object Module {
  def fromProto(mod: proto.Module): Module[NameWithType] = Module(
    pkg = mod.pkg.toList,
    name = mod.name,
    imports = mod.imports.toList.map(Import.fromProto),
    declarations = mod.declarations.toList.map(TopLevelDeclaration.fromProto),
    meta = NameWithType.fromProto(mod.getNameWithType),
  )
  implicit val moduleFunctor: Functor[Module] = new Functor[Module] {
    def map[A, B](ma: Module[A])(f: A => B): Module[B] = {
      ma.copy(
        meta = f(ma.meta),
        declarations = ma.declarations.map(_.map(f))
      )
    }
  }
}

sealed trait Import {
  def toProto: proto.Import = this match {
    case ImportModule(pkg, name) => proto.Import(pkg, name)
    case ImportSymbols(pkg, name, symbols) => proto.Import(pkg, name, Some(proto.Symbols(symbols)))
  }
}
object Import {
  def fromProto(imp: proto.Import): Import = imp.symbols match {
    case Some(syms) =>
      ImportSymbols(imp.pkg.toList, imp.name, syms.symbols.toList)
    case None =>
      ImportModule(imp.pkg.toList, imp.name)
  }
}

case class ImportModule(pkg: List[String], name: String) extends Import
case class ImportSymbols(pkg: List[String], name: String, symbols: List[String]) extends Import

sealed trait Expr[A] extends Tree[A] {
  def capturedVariables(implicit eqv: A =:= NamePosType): Set[Name] = this match {
    case LiteralInt(_, _) => Set.empty
    case LiteralLong(_, _) => Set.empty
    case LiteralFloat(_, _) => Set.empty
    case LiteralDouble(_, _) => Set.empty
    case LiteralBoolean(_, _) => Set.empty
    case LiteralString(_, _) => Set.empty
    case LiteralChar(_, _) => Set.empty
    case LiteralUnit(_) => Set.empty
    case Reference(_, meta) => Set(eqv(meta).name)
    case If(cond, thenExpr, elseExpr, _) =>
      cond.capturedVariables ++ thenExpr.capturedVariables ++ elseExpr.capturedVariables
    case Lambda(variables, body, _) =>
      body.capturedVariables -- variables.map(LocalName.apply)
    case Apply(fn, args, _) =>
      fn.capturedVariables ++ args.flatMap(_.capturedVariables)
  }

  def toProto(implicit eqv: A =:= NamePosType): proto.Expr = this match {
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
      proto.If(cond.toProto, thenExpr.toProto, elseExpr.toProto, nameWithType)
    case Lambda(variables, body, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Lambda(variables, body.toProto, nameWithType)
    case Apply(fn, args, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Apply(fn.toProto, args.map(_.toProto), nameWithType)
  }

  def substitute(subst: Map[TypeVariable, Type])(implicit eqv: A =:= NamePosType): Expr[A] =
    this.map(a => eqv(a).substitute(subst).asInstanceOf[A])
}
object Expr {
  def fromProto(expr: proto.Expr): Expr[NameWithType] = expr match {
    case int @ proto.LiteralInt(i, _) =>
      LiteralInt(i, NameWithType.fromProto(int.getNameWithType))
    case long @ proto.LiteralLong(l, _) =>
      LiteralLong(l, NameWithType.fromProto(long.getNameWithType))
    case flt @ proto.LiteralFloat(f, _) =>
      LiteralFloat(f, NameWithType.fromProto(flt.getNameWithType))
    case dbl @ proto.LiteralDouble(d, _) =>
      LiteralDouble(d, NameWithType.fromProto(dbl.getNameWithType))
    case bool @ proto.LiteralBoolean(b, _) =>
      LiteralBoolean(b, NameWithType.fromProto(bool.getNameWithType))
    case str @ proto.LiteralString(s, _) =>
      LiteralString(s, NameWithType.fromProto(str.getNameWithType))
    case char @ proto.LiteralChar(c, _) =>
      LiteralChar(c.charAt(0), NameWithType.fromProto(char.getNameWithType))
    case unit @ proto.LiteralUnit(_) =>
      LiteralUnit(NameWithType.fromProto(unit.getNameWithType))
    case ref @ proto.Reference(name, _) =>
      Reference(name, NameWithType.fromProto(ref.getNameWithType))
    case ifExpr @ proto.If(cond, thenExpr, elseExpr, _) =>
      If(
        Expr.fromProto(cond),
        Expr.fromProto(thenExpr),
        Expr.fromProto(elseExpr),
        NameWithType.fromProto(ifExpr.getNameWithType))
    case lambda @ proto.Lambda(variables, body, _) =>
      Lambda(
        variables.toList,
        Expr.fromProto(body),
        NameWithType.fromProto(lambda.getNameWithType))
    case app @ proto.Apply(fn, args, _) =>
      Apply(
        Expr.fromProto(fn),
        args.toList.map(Expr.fromProto),
        NameWithType.fromProto(app.getNameWithType))
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
      case ref @ Reference(_, _) =>
        ref.copy(meta = f(ref.meta))
      case ifExpr @ If(_, _, _, _) =>
        ifExpr.copy(
          cond = map(ifExpr.cond)(f),
          thenExpr = map(ifExpr.thenExpr)(f),
          elseExpr = map(ifExpr.elseExpr)(f),
          meta = f(ifExpr.meta))
      case lambda @ Lambda(_, _, _) =>
        lambda.copy(
          body = map(lambda.body)(f),
          meta = f(lambda.meta))
      case app @ Apply(_, _, _) =>
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
  meta: A
) extends Expr[A]

final case class Lambda[A](
  variables: List[String],
  body: Expr[A],
  meta: A
) extends Expr[A]

final case class Apply[A](
  fn: Expr[A],
  args: List[Expr[A]],
  meta: A
) extends Expr[A]

sealed trait Literal[A] extends Expr[A]

final case class LiteralInt[A](i: Int, meta: A) extends Literal[A]
final case class LiteralLong[A](l: Long, meta: A) extends Literal[A]
final case class LiteralFloat[A](f: Float, meta: A) extends Literal[A]
final case class LiteralDouble[A](d: Double, meta: A) extends Literal[A]
final case class LiteralBoolean[A](b: Boolean, meta: A) extends Literal[A]
final case class LiteralChar[A](c: Char, meta: A) extends Literal[A]
final case class LiteralString[A](s: String, meta: A) extends Literal[A]
final case class LiteralUnit[A](meta: A) extends Literal[A]

final case class Reference[A](name: String, meta: A) extends Expr[A]

sealed trait Declaration[A] extends Tree[A]
sealed trait TopLevelDeclaration[A] extends Declaration[A] {
  def name: String
  def toProto(implicit eqv: A =:= NamePosType): proto.TopLevelDeclaration = this match {
    case Let(name, expr, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Let(name, expr.toProto, nameWithType)
  }
  def substitute(subst: Map[TypeVariable, Type])(implicit eqv: A =:= NamePosType): TopLevelDeclaration[A] =
    this.map(a => eqv(a).substitute(subst).asInstanceOf[A])
}
object TopLevelDeclaration {
  def fromProto(decl: proto.TopLevelDeclaration): TopLevelDeclaration[NameWithType] = decl match {
    case let @ proto.Let(name, binding, _) =>
      Let(
        name,
        Expr.fromProto(binding),
        NameWithType.fromProto(let.getNameWithType))
    case proto.TopLevelDeclaration.Empty =>
      throw new Exception("Empty TopLevelDeclaration in protobuf")
  }
  implicit val topLevelDeclarationFunctor: Functor[TopLevelDeclaration] = new Functor[TopLevelDeclaration] {
    def map[A, B](ta: TopLevelDeclaration[A])(f: A => B): TopLevelDeclaration[B] = ta match {
      case let @ Let(_, _, _) =>
        let.copy(
          binding = let.binding.map(f),
          meta = f(let.meta))
    }
  }
}
sealed trait LocalDeclaration[A] extends Declaration[A]

final case class Let[A](name: String, binding: Expr[A], meta: A) extends TopLevelDeclaration[A] with LocalDeclaration[A]

sealed trait Name {
  def toProto: proto.Name = this match {
    case NoName => proto.NoName()
    case LocalName(nm) => proto.LocalName(nm)
    case ModuleName(pkg, cls) => proto.ModuleName(pkg, cls)
    case MemberName(pkg, cls, nm) => proto.MemberName(pkg, cls, nm)
  }
}
object Name {
  def fromProto(name: proto.Name): Name = name match {
    case proto.NoName() => NoName
    case proto.LocalName(nm) => LocalName(nm)
    case proto.ModuleName(pkg, cls) => ModuleName(pkg.toList, cls)
    case proto.MemberName(pkg, cls, nm) => MemberName(pkg.toList, cls, nm)
    case proto.Name.Empty => throw new Exception("Empty Name in protobuf")
  }
}

case object NoName extends Name
case class LocalName(name: String) extends Name
case class ModuleName(pkg: List[String], cls: String) extends Name
case class MemberName(pkg: List[String], cls: String, name: String) extends Name

case class TypeScheme(bound: List[TypeVariable], typ: Type) {
  def toProto: proto.TypeScheme =
    proto.TypeScheme(bound.map(tv => proto.TypeVariable(tv.id)), typ.toProto)

  def freeTypeVariables: Set[TypeVariable] =
    typ.freeTypeVariables diff bound.toSet

  def substitute(subst: Map[TypeVariable, Type]) =
    TypeScheme(bound, typ.substitute(subst -- bound))

  def instantiate: Type = if (bound.isEmpty) typ else {
    val freshVars = bound.map(_ => TypeVariable())
    val subst = bound.zip(freshVars).toMap
    scribe.info("instantiate: " + Printer.print(subst))
    typ.substitute(subst)
  }
}
object TypeScheme {
  def apply(typ: Type): TypeScheme = TypeScheme(List.empty, typ)

  def generalize(env: Map[String, TypeScheme], typ: Type): TypeScheme = {
    val freeInEnv = env.values.flatMap(_.freeTypeVariables).toSet
    val bound = typ.freeTypeVariables diff freeInEnv
    val scheme = TypeScheme(bound.toList, typ)
    scribe.info("generalize: " + bound.map(Printer.print(_)).mkString("[", ", ", "]"))
    scheme
  }

  def fromProto(typ: proto.TypeScheme) = typ match {
    case proto.TypeScheme(bound, t) =>
      // Instantiate the type scheme with fresh type variables for this compilation run
      val typ = Type.fromProto(t)
      val freshVars = bound.toList.map(_ => TypeVariable())
      val tyVars = bound.toList.map(TypeVariable.fromProto)
      val subst = tyVars.zip(freshVars).toMap
      TypeScheme(tyVars, typ.substitute(subst))
  }
}

sealed trait Type {
  def toProto: proto.Type = this match {
    case TypeVariable(i) => proto.TypeVariable(i)
    case TypeConstructor(name, tyParams) => proto.TypeConstructor(name, tyParams.map(_.toProto))
  }

  def isPrimitive = this match {
    case TypeConstructor(name, _) if Type.primitives.contains(name) =>
      true
    case _ =>
      false
  }

  def freeTypeVariables: Set[TypeVariable] = this match {
    case tyVar @ TypeVariable(_) => Set(tyVar)
    case TypeConstructor(_, tyParams) =>
      tyParams.flatMap(_.freeTypeVariables).toSet
  }

  def substitute(subst: Map[TypeVariable, Type]): Type = this match {
    case tyVar @ TypeVariable(_) =>
      val substFor = subst.getOrElse(tyVar, tyVar)
      if (tyVar != substFor)
        scribe.info(Printer.print(tyVar) + " |-> " + Printer.print(substFor))
      substFor
    case TypeConstructor(nm, tyParams) =>
      TypeConstructor(nm, tyParams.map(_.substitute(subst)))
  }
}

object Type {
  val UnitClass = "inc.rts.Unit"

  val primitives = Set("Int", "Long", "Float", "Double", "Boolean", "Char")

  val Int = TypeConstructor("Int", List.empty)
  val Long = TypeConstructor("Long", List.empty)
  val Float = TypeConstructor("Float", List.empty)
  val Double = TypeConstructor("Double", List.empty)
  val Boolean = TypeConstructor("Boolean", List.empty)
  val Char = TypeConstructor("Char", List.empty)
  val String = TypeConstructor("String", List.empty)
  val Module = TypeConstructor("Module", List.empty)
  val Unit = TypeConstructor(UnitClass, List.empty)
  def Function(from: List[Type], to: Type) = TypeConstructor("->", from ++ List(to))

  def fromProto(typ: proto.Type): Type = typ match {
    case proto.TypeVariable(id) =>
      TypeVariable(id)
    case proto.TypeConstructor(name, typeParams) =>
      TypeConstructor(name, typeParams.toList.map(Type.fromProto))
    case proto.Type.Empty =>
      throw new Exception("Empty Type in protobuf")
  }
}

case class TypeVariable(id: Int) extends Type {
  def occursIn(typ: Type) = typ.freeTypeVariables.contains(this)
}

object TypeVariable {
  def fromProto(tyVar: proto.TypeVariable) = TypeVariable(tyVar.id)
  val nextVariableId = new AtomicInteger(1)
  def apply(): TypeVariable = TypeVariable(nextVariableId.getAndIncrement)
}

case class TypeConstructor(name: String, typeParams: List[Type]) extends Type

case class NameWithPos(name: Name, pos: Pos) {
  def withType(typ: TypeScheme): NamePosType =
    NamePosType(name, pos, typ)
  def withSimpleType(typ: Type): NamePosType =
    withType(TypeScheme(typ))
}

case class NameWithType(name: Name, typ: TypeScheme) {
  def withEmptyPos: NamePosType =
    NamePosType(name, Pos.Empty, typ)

  def substitute(subst: Map[TypeVariable, Type]): NameWithType =
    copy(typ = typ.substitute(subst))
}

object NameWithType {
  def fromProto(nameWithType: proto.NameWithType): NameWithType =
    NameWithType(
      Name.fromProto(nameWithType.name),
      TypeScheme.fromProto(nameWithType.getType)
    )
}

case class NamePosType(name: Name, pos: Pos, typ: TypeScheme) {
  def toProto = proto.NameWithType(
    name = name.toProto,
    `type` = Some(typ.toProto)
  )

  def forgetPos = NameWithType(name, typ)

  def substitute(subst: Map[TypeVariable, Type]): NamePosType =
    copy(typ = typ.substitute(subst))
}
