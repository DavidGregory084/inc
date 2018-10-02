package inc.common

import java.util.concurrent.atomic.AtomicInteger

abstract class Error(val msg: String) extends Throwable(msg) with Product with Serializable

sealed trait Tree[A] extends Product with Serializable {
  def meta: A
  def pos: Option[(Int, Int)]
}

final case class Module[A](
  pkg: List[String],
  name: String,
  imports: List[Import],
  declarations: List[TopLevelDeclaration[A]],
  meta: A,
  pos: Option[(Int, Int)]
) extends Tree[A] {
  def toProto(implicit eqv: A =:= NameWithType): proto.Module = proto.Module(
    pkg = pkg,
    name = name,
    imports = imports.map(_.toProto),
    declarations = declarations.map(_.toProto),
    nameWithType = Some(eqv(meta).toProto)
  )

  def substitute(subst: Map[TypeVariable, Type])(implicit eqv: A =:= NameWithType): Module[A] = {
    val nameWithType = eqv(meta)
    copy(
      declarations = declarations.map(_.substitute(subst)),
      meta = nameWithType.substitute(subst).asInstanceOf[A])
  }
}
object Module {
  def fromProto(mod: proto.Module): Module[NameWithType] = Module(
    pkg = mod.pkg.toList,
    name = mod.name,
    imports = mod.imports.toList.map(Import.fromProto),
    declarations = mod.declarations.toList.map(TopLevelDeclaration.fromProto),
    meta = NameWithType.fromProto(mod.getNameWithType),
    pos = None
  )
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
  def capturedVariables(implicit eqv: A =:= NameWithType): Set[Name] = this match {
    case LiteralInt(_, _, _) => Set.empty
    case LiteralLong(_, _, _) => Set.empty
    case LiteralFloat(_, _, _) => Set.empty
    case LiteralDouble(_, _, _) => Set.empty
    case LiteralBoolean(_, _, _) => Set.empty
    case LiteralString(_, _, _) => Set.empty
    case LiteralChar(_, _, _) => Set.empty
    case LiteralUnit(_, _) => Set.empty
    case Reference(_, meta, _) => Set(eqv(meta).name)
    case If(cond, thenExpr, elseExpr, _, _) =>
      cond.capturedVariables ++ thenExpr.capturedVariables ++ elseExpr.capturedVariables
    case Lambda(variables, body, _, _) =>
      body.capturedVariables -- variables.map(LocalName.apply)
    case Apply(fn, args, _, _) =>
      fn.capturedVariables ++ args.flatMap(_.capturedVariables)
  }

  def toProto(implicit eqv: A =:= NameWithType): proto.Expr = this match {
    case LiteralInt(i, meta, _) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralInt(i, nameWithType)
    case LiteralLong(l, meta, _) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralLong(l, nameWithType)
    case LiteralFloat(f, meta, _) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralFloat(f, nameWithType)
    case LiteralDouble(d, meta, _) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralDouble(d, nameWithType)
    case LiteralBoolean(b, meta, _) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralBoolean(b, nameWithType)
    case LiteralString(s, meta, _) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralString(s, nameWithType)
    case LiteralChar(c, meta, _) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralChar(c.toString, nameWithType)
    case LiteralUnit(meta, _) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.LiteralUnit(nameWithType)
    case Reference(name, meta, _) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Reference(name, nameWithType)
    case If(cond, thenExpr, elseExpr, meta, _) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.If(cond.toProto, thenExpr.toProto, elseExpr.toProto, nameWithType)
    case Lambda(variables, body, meta, _) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Lambda(variables, body.toProto, nameWithType)
    case Apply(fn, args, meta, _) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Apply(fn.toProto, args.map(_.toProto), nameWithType)
  }

  def substitute(subst: Map[TypeVariable, Type])(implicit eqv: A =:= NameWithType): Expr[A] = this match {
    case int @ LiteralInt(_, meta, _) =>
      val nameWithType = eqv(meta)
      int.copy(meta = nameWithType.substitute(subst).asInstanceOf[A])
    case long @ LiteralLong(_, meta, _) =>
      val nameWithType = eqv(meta)
      long.copy(meta = nameWithType.substitute(subst).asInstanceOf[A])
    case float @ LiteralFloat(_, meta, _) =>
      val nameWithType = eqv(meta)
      float.copy(meta = nameWithType.substitute(subst).asInstanceOf[A])
    case double @ LiteralDouble(_, meta, _) =>
      val nameWithType = eqv(meta)
      double.copy(meta = nameWithType.substitute(subst).asInstanceOf[A])
    case boolean @ LiteralBoolean(_, meta, _) =>
      val nameWithType = eqv(meta)
      boolean.copy(meta = nameWithType.substitute(subst).asInstanceOf[A])
    case string @ LiteralString(_, meta, _) =>
      val nameWithType = eqv(meta)
      string.copy(meta = nameWithType.substitute(subst).asInstanceOf[A])
    case char @ LiteralChar(_, meta, _) =>
      val nameWithType = eqv(meta)
      char.copy(meta = nameWithType.substitute(subst).asInstanceOf[A])
    case unit @ LiteralUnit(meta, _) =>
      val nameWithType = eqv(meta)
      unit.copy(meta = nameWithType.substitute(subst).asInstanceOf[A])
    case ref @ Reference(_, meta, _) =>
      val nameWithType = eqv(meta)
      ref.copy(meta = nameWithType.substitute(subst).asInstanceOf[A])
    case ifExpr @ If(cond, thenExpr, elseExpr, meta, _) =>
      val nameWithType = eqv(meta)
      ifExpr.copy(
        cond = cond.substitute(subst),
        thenExpr = thenExpr.substitute(subst),
        elseExpr = elseExpr.substitute(subst),
        meta = nameWithType.substitute(subst).asInstanceOf[A])
    case lambda @ Lambda(_, body, meta, _) =>
      val nameWithType = eqv(meta)
      lambda.copy(
        body = body.substitute(subst),
        meta = nameWithType.substitute(subst).asInstanceOf[A])
    case apply @ Apply(fn, args, meta, _) =>
      val nameWithType = eqv(meta)
      apply.copy(
        fn = fn.substitute(subst),
        args = args.map(_.substitute(subst)),
        meta = nameWithType.substitute(subst).asInstanceOf[A])
  }
}
object Expr {
  def fromProto(expr: proto.Expr): Expr[NameWithType] = expr match {
    case int @ proto.LiteralInt(i, _) =>
      LiteralInt(i, NameWithType.fromProto(int.getNameWithType), None)
    case long @ proto.LiteralLong(l, _) =>
      LiteralLong(l, NameWithType.fromProto(long.getNameWithType), None)
    case flt @ proto.LiteralFloat(f, _) =>
      LiteralFloat(f, NameWithType.fromProto(flt.getNameWithType), None)
    case dbl @ proto.LiteralDouble(d, _) =>
      LiteralDouble(d, NameWithType.fromProto(dbl.getNameWithType), None)
    case bool @ proto.LiteralBoolean(b, _) =>
      LiteralBoolean(b, NameWithType.fromProto(bool.getNameWithType), None)
    case str @ proto.LiteralString(s, _) =>
      LiteralString(s, NameWithType.fromProto(str.getNameWithType), None)
    case char @ proto.LiteralChar(c, _) =>
      LiteralChar(c.charAt(0), NameWithType.fromProto(char.getNameWithType), None)
    case unit @ proto.LiteralUnit(_) =>
      LiteralUnit(NameWithType.fromProto(unit.getNameWithType), None)
    case ref @ proto.Reference(name, _) =>
      Reference(name, NameWithType.fromProto(ref.getNameWithType), None)
    case ifExpr @ proto.If(cond, thenExpr, elseExpr, _) =>
      If(
        Expr.fromProto(cond),
        Expr.fromProto(thenExpr),
        Expr.fromProto(elseExpr),
        NameWithType.fromProto(ifExpr.getNameWithType),
        None
      )
    case lambda @ proto.Lambda(variables, body, _) =>
      Lambda(
        variables.toList,
        Expr.fromProto(body),
        NameWithType.fromProto(lambda.getNameWithType),
        None
      )
    case app @ proto.Apply(fn, args, _) =>
      Apply(
        Expr.fromProto(fn),
        args.toList.map(Expr.fromProto),
        NameWithType.fromProto(app.getNameWithType),
        None
      )
    case proto.Expr.Empty =>
      throw new Exception("Empty Expr in protobuf")
  }
}

final case class If[A](
  cond: Expr[A],
  thenExpr: Expr[A],
  elseExpr: Expr[A],
  meta: A,
  pos: Option[(Int, Int)]
) extends Expr[A]

final case class Lambda[A](
  variables: List[String],
  body: Expr[A],
  meta: A,
  pos: Option[(Int, Int)]
) extends Expr[A]

final case class Apply[A](
  fn: Expr[A],
  args: List[Expr[A]],
  meta: A,
  pos: Option[(Int, Int)]
) extends Expr[A]

sealed trait Literal[A] extends Expr[A]

final case class LiteralInt[A](i: Int, meta: A, pos: Option[(Int, Int)]) extends Literal[A]
final case class LiteralLong[A](l: Long, meta: A, pos: Option[(Int, Int)]) extends Literal[A]
final case class LiteralFloat[A](f: Float, meta: A, pos: Option[(Int, Int)]) extends Literal[A]
final case class LiteralDouble[A](d: Double, meta: A, pos: Option[(Int, Int)]) extends Literal[A]
final case class LiteralBoolean[A](b: Boolean, meta: A, pos: Option[(Int, Int)]) extends Literal[A]
final case class LiteralChar[A](c: Char, meta: A, pos: Option[(Int, Int)]) extends Literal[A]
final case class LiteralString[A](s: String, meta: A, pos: Option[(Int, Int)]) extends Literal[A]
final case class LiteralUnit[A](meta: A, pos: Option[(Int, Int)]) extends Literal[A]

final case class Reference[A](name: String, meta: A, pos: Option[(Int, Int)]) extends Expr[A]

sealed trait Declaration[A] extends Tree[A]
sealed trait TopLevelDeclaration[A] extends Declaration[A] {
  def name: String
  def toProto(implicit eqv: A =:= NameWithType): proto.TopLevelDeclaration = this match {
    case Let(name, expr, meta, _) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Let(name, expr.toProto, nameWithType)
  }
  def substitute(subst: Map[TypeVariable, Type])(implicit eqv: A =:= NameWithType): TopLevelDeclaration[A] = this match {
    case let @ Let(_, expr, meta, _) =>
      val nameWithType = eqv(meta)
      let.copy(
        binding = expr.substitute(subst),
        meta = nameWithType.substitute(subst).asInstanceOf[A])
  }
}
object TopLevelDeclaration {
  def fromProto(decl: proto.TopLevelDeclaration): TopLevelDeclaration[NameWithType] = decl match {
    case let @ proto.Let(name, binding, _) =>
      Let(
        name,
        Expr.fromProto(binding),
        NameWithType.fromProto(let.getNameWithType),
        None
      )
    case proto.TopLevelDeclaration.Empty =>
      throw new Exception("Empty TopLevelDeclaration in protobuf")
  }
}
sealed trait LocalDeclaration[A] extends Declaration[A]

final case class Let[A](name: String, binding: Expr[A], meta: A, pos: Option[(Int, Int)]) extends TopLevelDeclaration[A] with LocalDeclaration[A]

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

    val printed = subst.map {
      case (tyVar, typ) =>
        s"${Printer.print(tyVar)} |-> ${Printer.print(typ)}"
    }.mkString(", ")

    println("instantiate: "+printed)

    typ.substitute(subst)
  }
}
object TypeScheme {
  def apply(typ: Type): TypeScheme = TypeScheme(List.empty, typ)

  def generalize(env: Map[String, TypeScheme], typ: Type): TypeScheme = {
    val freeInEnv = env.values.flatMap(_.freeTypeVariables).toSet
    val bound = typ.freeTypeVariables diff freeInEnv
    val scheme = TypeScheme(bound.toList, typ)
    println("generalize: " + bound.map(Printer.print(_)).mkString("[", ", ", "]"))
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
        println(Printer.print(tyVar) + " |-> " + Printer.print(substFor))
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

case class NameWithType(name: Name, typ: TypeScheme) {
  def toProto = proto.NameWithType(
    name = name.toProto,
    `type` = Some(typ.toProto)
  )

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
