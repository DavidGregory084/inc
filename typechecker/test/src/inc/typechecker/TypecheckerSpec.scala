package inc.typechecker

import inc.common._
import org.scalatest._

class TypecheckerSpec extends FlatSpec with Matchers {
  def mkModule(name: String, decls: List[TopLevelDeclaration[Name]]) = Module(
    pkg = List("Test", "Typechecker"),
    name = name,
    imports = List.empty,
    declarations = decls,
    pos = Pos.Empty,
    meta = ModuleName(List("Test", "Typechecker"), name)
  )

  def mkLet(name: String, binding: Expr[Name]) =
    Let(name, binding, Pos.Empty, LocalName(name))

  def mkInt(i: Int) = LiteralInt(i, Pos.Empty, NoName: Name)
  def mkDbl(d: Double) = LiteralDouble(d, Pos.Empty, NoName: Name)
  def mkBool(b: Boolean) = LiteralBoolean(b, Pos.Empty, NoName: Name)
  def mkRef(r: String) = Reference(r, Pos.Empty, NoName: Name)

  def mkIf(cond: Expr[Name], thenExpr: Expr[Name], elseExpr: Expr[Name]) =
    If(cond, thenExpr, elseExpr, Pos.Empty, NoName: Name)

  def mkLam(params: List[String], body: Expr[Name]) = Lambda(
    params.map(nm => Param(nm, Pos.Empty, LocalName(nm): Name)),
    body,
    Pos.Empty,
    NoName: Name
  )

  def mkApp(fn: Expr[Name], args: List[Expr[Name]]) =
    Apply(fn, args, Pos.Empty, NoName)

  def mkCheckedModule(name: String, decls: List[TopLevelDeclaration[NameWithType]]) = Module(
    pkg = List("Test", "Typechecker"),
    name = name,
    imports = List.empty,
    declarations = decls,
    pos = Pos.Empty,
    meta = NameWithType(ModuleName(List("Test", "Typechecker"), name), TypeScheme(Type.Module)))

  def mkCheckedLet(name: String, binding: Expr[NameWithType]) =
    Let(name, binding, Pos.Empty, NameWithType(LocalName(name), binding.meta.typ))

  def mkCheckedInt(i: Int) = LiteralInt(i, Pos.Empty, NameWithType(NoName, TypeScheme(Type.Int)))
  def mkCheckedRef(r: String, typ: TypeScheme) = Reference(r, Pos.Empty, NameWithType(NoName, typ))

  val noImports = Map.empty[String, TopLevelDeclaration[NameWithType]]

  "Typechecker" should "typecheck let bound literals successfully" in {
    val mod = mkModule("Int", List(mkLet("int", mkInt(42))))
    val expected = mkCheckedModule("Int", List(mkCheckedLet("int", mkCheckedInt(42))))
    val result = Typechecker.typecheck(mod, noImports, "")
    result shouldBe 'right
    result.right.get shouldBe expected
  }

  it should "typecheck let bound field references successfully" in {
    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int2", mkRef("int"))
    ))
    val expected = mkCheckedModule("Ref", List(
      mkCheckedLet("int", mkCheckedInt(42)),
      mkCheckedLet("int2", mkCheckedRef("int", TypeScheme(Type.Int)))
    ))
    val result = Typechecker.typecheck(mod, noImports, "")
    result shouldBe 'right
    result.right.get shouldBe expected
  }

  it should "return an error when there is a reference to a field which doesn't exist" in {
    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int2", mkRef("int3"))
    ))
    val result = Typechecker.typecheck(mod, noImports, "")
    result shouldBe 'left
  }

  it should "ensure the expression provided to if is a Boolean" in {
    val mod1 = mkModule("If", List(
      mkLet("integer", mkIf(mkBool(true), mkInt(42), mkInt(41)))
    ))

    val result1 = Typechecker.typecheck(mod1, noImports, "")
    result1 shouldBe 'right

    val mod2 = mkModule("If", List(
      mkLet("integer", mkIf(mkInt(1), mkInt(42), mkInt(41)))
    ))

    val result2 = Typechecker.typecheck(mod2, noImports, "")
    result2 shouldBe 'left
    result2.left.get.head shouldBe TypeError(Pos.Empty, Red("Int") + " does not unify with " + Red("Boolean"))
  }

  it should "ensure the expressions provided to both branches of an if are compatible" in {
    val mod1 = mkModule("If", List(
      mkLet("integer", mkIf(mkBool(true), mkInt(42), mkInt(41)))
    ))

    val result1 = Typechecker.typecheck(mod1, noImports, "")
    result1 shouldBe 'right

    val mod2 = mkModule("If", List(
      mkLet("integer", mkIf(mkBool(true), mkInt(42), mkDbl(41.0)))
    ))

    val result2 = Typechecker.typecheck(mod2, noImports, "")
    result2 shouldBe 'left
    result2.left.get.head shouldBe TypeError(Pos.Empty, Red("Int") + " does not unify with " + Red("Double"))
  }

  it should "infer the parameter and return type of lambda expressions" in {
    val mod1 = mkModule("If", List(
      mkLet("lam", mkLam(List("bool"), mkIf(mkRef("bool"), mkInt(42), mkInt(41))))
    ))

    val result = Typechecker.typecheck(mod1, noImports, "")
    result shouldBe 'right

    val mod2 = mkModule("Lambda", List(
      mkLet("lam", mkLam(List("a"), mkRef("a")))
    ))

    val result2 = Typechecker.typecheck(mod2, noImports, "")
    result2 shouldBe 'right
  }

  it should "infer the type of lambda application" in {
    val mod1 = mkModule("Apply", List(
      mkLet("lam", mkLam(List("bool"), mkIf(mkRef("bool"), mkInt(42), mkInt(41)))),
      mkLet("app", mkApp(mkRef("lam"), List(mkBool(true))))
    ))

    val result1 = Typechecker.typecheck(mod1, noImports, "")
    result1 shouldBe 'right

    val mod2 = mkModule("Apply", List(
      mkLet("lam", mkLam(List("a"), mkRef("a"))),
      mkLet("app", mkApp(mkRef("lam"), List(mkBool(true))))
    ))

    val result2 = Typechecker.typecheck(mod2, noImports, "")
    result2 shouldBe 'right
  }
}
