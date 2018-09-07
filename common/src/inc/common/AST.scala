package inc.common

abstract class Error(val msg: String) extends Throwable(msg) with Product with Serializable

sealed trait Tree[A] extends Product with Serializable {
  def meta: A
}

final case class Module[A](
  pkg: List[String],
  name: String,
  imports: List[Import],
  declarations: List[TopLevelDeclaration[A]],
  meta: A
) extends Tree[A] {
  def toProto(implicit eqv: A =:= NameWithType): proto.Module = proto.Module(
    pkg = pkg,
    name = name,
    imports = imports.map(_.toProto),
    declarations = declarations.map(_.toProto),
    nameWithType = Some(eqv(meta).toProto)
  )
}
object Module {
  def fromProto(mod: proto.Module): Module[NameWithType] = Module(
    pkg = mod.pkg.toList,
    name = mod.name,
    imports = mod.imports.toList.map(Import.fromProto),
    declarations = mod.declarations.toList.map(TopLevelDeclaration.fromProto),
    meta = NameWithType.fromProto(mod.nameWithType.getOrElse(throw new Exception("No type in protobuf")))
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
  def toProto(implicit eqv: A =:= NameWithType): proto.Expr = this match {
    case LiteralInt(i, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Expr(nameWithType, proto.Expr.ExprType.Int(proto.LiteralInt(i, nameWithType)))
    case LiteralLong(l, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Expr(nameWithType, proto.Expr.ExprType.Long(proto.LiteralLong(l, nameWithType)))
    case LiteralFloat(f, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Expr(nameWithType, proto.Expr.ExprType.Flt(proto.LiteralFloat(f, nameWithType)))
    case LiteralDouble(d, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Expr(nameWithType, proto.Expr.ExprType.Dbl(proto.LiteralDouble(d, nameWithType)))
    case LiteralBoolean(b, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Expr(nameWithType, proto.Expr.ExprType.Boolean(proto.LiteralBoolean(b, nameWithType)))
    case LiteralString(s, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Expr(nameWithType, proto.Expr.ExprType.Str(proto.LiteralString(s, nameWithType)))
    case LiteralChar(c, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Expr(nameWithType, proto.Expr.ExprType.Char(proto.LiteralChar(c.toString, nameWithType)))
    case LiteralUnit(meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Expr(nameWithType, proto.Expr.ExprType.Unit(proto.LiteralUnit(nameWithType)))
    case Reference(name, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Expr(nameWithType, proto.Expr.ExprType.Ref(proto.Reference(name, nameWithType)))
    case If(cond, thenExpr, elseExpr, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.Expr(nameWithType, proto.Expr.ExprType.If(proto.If(Some(cond.toProto), Some(thenExpr.toProto), Some(elseExpr.toProto), nameWithType)))
  }
}
object Expr {
  def fromProto(expr: proto.Expr): Expr[NameWithType] = expr.exprType match {
    case proto.Expr.ExprType.Int(proto.LiteralInt(i, nameWithType)) =>
      LiteralInt(i, NameWithType.fromProto(nameWithType.getOrElse(throw new Exception("No type in protobuf"))))
    case proto.Expr.ExprType.Long(proto.LiteralLong(l, nameWithType)) =>
      LiteralLong(l, NameWithType.fromProto(nameWithType.getOrElse(throw new Exception("No type in protobuf"))))
    case proto.Expr.ExprType.Flt(proto.LiteralFloat(f, nameWithType)) =>
      LiteralFloat(f, NameWithType.fromProto(nameWithType.getOrElse(throw new Exception("No type in protobuf"))))
    case proto.Expr.ExprType.Dbl(proto.LiteralDouble(d, nameWithType)) =>
      LiteralDouble(d, NameWithType.fromProto(nameWithType.getOrElse(throw new Exception("No type in protobuf"))))
    case proto.Expr.ExprType.Boolean(proto.LiteralBoolean(b, nameWithType)) =>
      LiteralBoolean(b, NameWithType.fromProto(nameWithType.getOrElse(throw new Exception("No type in protobuf"))))
    case proto.Expr.ExprType.Str(proto.LiteralString(s, nameWithType)) =>
      LiteralString(s, NameWithType.fromProto(nameWithType.getOrElse(throw new Exception("No type in protobuf"))))
    case proto.Expr.ExprType.Char(proto.LiteralChar(c, nameWithType)) =>
      LiteralChar(c.charAt(0), NameWithType.fromProto(nameWithType.getOrElse(throw new Exception("No type in protobuf"))))
    case proto.Expr.ExprType.Unit(proto.LiteralUnit(nameWithType)) =>
      LiteralUnit(NameWithType.fromProto(nameWithType.getOrElse(throw new Exception("No type in protobuf"))))
    case proto.Expr.ExprType.Ref(proto.Reference(name, nameWithType)) =>
      Reference(name, NameWithType.fromProto(nameWithType.getOrElse(throw new Exception("No type in protobuf"))))
    case proto.Expr.ExprType.If(proto.If(cond, thenExpr, elseExpr, nameWithType)) =>
      If(
        Expr.fromProto(cond.getOrElse(throw new Exception("No if condition in protobuf"))),
        Expr.fromProto(thenExpr.getOrElse(throw new Exception("No then expression in protobuf"))),
        Expr.fromProto(elseExpr.getOrElse(throw new Exception("No else expression in protobuf"))),
        NameWithType.fromProto(nameWithType.getOrElse(throw new Exception("No type in protobuf")))
      )
    case proto.Expr.ExprType.Empty =>
      throw new Exception("Empty Expr in protobuf")
  }
}

final case class If[A](
  cond: Expr[A],
  thenExpr: Expr[A],
  elseExpr: Expr[A],
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
  def toProto(implicit eqv: A =:= NameWithType): proto.TopLevelDeclaration = this match {
    case Let(name, expr, meta) =>
      val nameWithType = Some(eqv(meta).toProto)
      proto.TopLevelDeclaration(proto.TopLevelDeclaration.DeclarationType.Let(proto.Let(name, Some(expr.toProto), nameWithType)))
  }
}
object TopLevelDeclaration {
  def fromProto(decl: proto.TopLevelDeclaration): TopLevelDeclaration[NameWithType] = decl.declarationType match {
    case proto.TopLevelDeclaration.DeclarationType.Let(proto.Let(name, binding, nameWithType)) => Let(
      name,
      Expr.fromProto(binding.getOrElse(throw new Exception("No expression bound to Let in protobuf"))),
      NameWithType.fromProto(nameWithType.getOrElse(throw new Exception("No type in protobuf")))
    )
    case proto.TopLevelDeclaration.DeclarationType.Empty =>
      throw new Exception("Empty TopLevelDeclaration in protobuf")
  }
}
sealed trait LocalDeclaration[A] extends Declaration[A]

final case class Let[A](name: String, binding: Expr[A], meta: A) extends TopLevelDeclaration[A] with LocalDeclaration[A]

// final case class Def(name: String, params: Seq[Param], body: Expr) extends TopLevelDeclaration

sealed trait Name {
  def toProto: proto.Name = this match {
    case NoName => proto.Name(proto.Name.NameType.NoName(proto.NoName()))
    case LocalName(nm) => proto.Name(proto.Name.NameType.LocalName(proto.LocalName(nm)))
    case ModuleName(pkg, cls) => proto.Name(proto.Name.NameType.ModuleName(proto.ModuleName(pkg, cls)))
    case MemberName(pkg, cls, nm) => proto.Name(proto.Name.NameType.MemberName(proto.MemberName(pkg, cls, nm)))
  }
}
object Name {
  def fromProto(name: proto.Name): Name = name.nameType match {
    case proto.Name.NameType.NoName(_) =>
      NoName
    case proto.Name.NameType.LocalName(proto.LocalName(nm)) =>
      LocalName(nm)
    case proto.Name.NameType.ModuleName(proto.ModuleName(pkg, cls)) =>
      ModuleName(pkg.toList, cls)
    case proto.Name.NameType.MemberName(proto.MemberName(pkg, cls, nm)) =>
      MemberName(pkg.toList, cls, nm)
    case proto.Name.NameType.Empty =>
      throw new Exception("Empty Name in protobuf")
  }
}

case object NoName extends Name
case class LocalName(name: String) extends Name
case class ModuleName(pkg: List[String], cls: String) extends Name
case class MemberName(pkg: List[String], cls: String, name: String) extends Name

sealed trait Type {
  def toProto: proto.Type = this match {
    // case TypeVariable(i, _) =>
    //   proto.Type(proto.Type.TypeType.TyVar(proto.TypeVariable(i)))
    case TypeConstructor(name, tyParams) =>
      proto.Type(proto.Type.TypeType.TyCon(proto.TypeConstructor(name, tyParams.map(_.toProto))))
  }
}

object Type {
  val UnitClass = "inc.rts.Unit"

  val Int = TypeConstructor("Int", List.empty)
  val Long = TypeConstructor("Long", List.empty)
  val Float = TypeConstructor("Float", List.empty)
  val Double = TypeConstructor("Double", List.empty)
  val Boolean = TypeConstructor("Boolean", List.empty)
  val Char = TypeConstructor("Char", List.empty)
  val String = TypeConstructor("String", List.empty)
  val Module = TypeConstructor("Module", List.empty)
  val Unit = TypeConstructor(UnitClass, List.empty)
  def Function(from: Type, to: Type) = TypeConstructor("->", List(from, to))

  def fromProto(typ: proto.Type): Type = typ.typeType match {
    // case proto.Type.TypeType.TyVar(proto.TypeVariable(id)) =>
    //   TypeVariable(id)
    case proto.Type.TypeType.TyCon(proto.TypeConstructor(name, typeParams)) =>
      TypeConstructor(name, typeParams.toList.map(Type.fromProto))
    case proto.Type.TypeType.Empty =>
      throw new Exception("Empty Type in protobuf")
  }
}

// case class TypeVariable(id: Int, name: String) extends Type

// object TypeVariable {
//   var nextVariableId = 0

//   def apply(id: Int): TypeVariable = TypeVariable(id, "A" + id)

//   def apply(): TypeVariable = {
//     val tyVar = TypeVariable(nextVariableId, "A" + nextVariableId)
//     nextVariableId += 1
//     tyVar
//   }
// }

case class TypeConstructor(name: String, typeParams: List[Type]) extends Type

case class NameWithType(name: Name, typ: Type) {
  def toProto = proto.NameWithType(
    name = Some(name.toProto),
    `type` = Some(typ.toProto)
  )
}

object NameWithType {
  def fromProto(nameWithType: proto.NameWithType): NameWithType =
    NameWithType(
      Name.fromProto(nameWithType.name.getOrElse(throw new Exception("No name in protobuf"))),
      Type.fromProto(nameWithType.`type`.getOrElse(throw new Exception("No type in protobuf")))
    )
}
