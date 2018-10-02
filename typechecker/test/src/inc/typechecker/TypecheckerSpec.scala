package inc.typechecker

import inc.common._
import org.scalatest._

class TypecheckerSpec extends FlatSpec with Matchers {
  def mkModule(name: String, decls: List[TopLevelDeclaration[Name]]) = Module(
    pkg = List("Test", "Typechecker"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = ModuleName(List("Test", "Typechecker"), name),
    pos = None)

  def mkCheckedModule(name: String, decls: List[TopLevelDeclaration[NameWithType]]) = Module(
    pkg = List("Test", "Typechecker"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = NameWithType(ModuleName(List("Test", "Typechecker"), name), TypeScheme(Type.Module)),
    pos = None)

  "Typechecker" should "typecheck let bound literals successfully" in {
    val mod = mkModule("Int", List(Let("int", LiteralInt(42, NoName, None), LocalName("int"), None)))
    val expected = mkCheckedModule("Int", List(
      Let("int", LiteralInt(42, NameWithType(NoName, TypeScheme(Type.Int)), None), NameWithType(LocalName("int"), TypeScheme(Type.Int)), None)
    ))
    val result = Typechecker.typecheck(mod)
    result shouldBe 'right
    result.right.get shouldBe expected
  }

  it should "typecheck let bound field references successfully" in {
    val mod = mkModule("Ref", List(
      Let("int", LiteralInt(42, NoName, None), LocalName("int"), None),
      Let("int2", Reference("int", NoName, None), LocalName("int2"), None)
    ))
    val expected = mkCheckedModule("Ref", List(
      Let("int", LiteralInt(42, NameWithType(NoName, TypeScheme(Type.Int)), None), NameWithType(LocalName("int"), TypeScheme(Type.Int)), None),
      Let("int2", Reference("int", NameWithType(NoName, TypeScheme(Type.Int)), None), NameWithType(LocalName("int2"), TypeScheme(Type.Int)), None)
    ))
    val result = Typechecker.typecheck(mod)
    result shouldBe 'right
    result.right.get shouldBe expected
  }

  it should "return an error when there is a reference to a field which doesn't exist" in {
    val mod = mkModule("Ref", List(
      Let("int", LiteralInt(42, NoName, None), LocalName("int"), None),
      Let("int2", Reference("int3", NoName, None), LocalName("int2"), None)
    ))
    val result = Typechecker.typecheck(mod)
    result shouldBe 'left
  }

  it should "ensure the expression provided to if is a Boolean" in {
    val mod1 = mkModule("If", List(
      Let("integer", If(
        LiteralBoolean(true, NoName, None),
        LiteralInt(42, NoName, None),
        LiteralInt(41, NoName, None),
        NoName, None
      ), LocalName("integer"), None)
    ))

    val result1 = Typechecker.typecheck(mod1)
    result1 shouldBe 'right

    val mod2 = mkModule("If", List(
      Let("integer", If(
        LiteralInt(1, NoName, None),
        LiteralInt(42, NoName, None),
        LiteralInt(41, NoName, None),
        NoName, None
      ), LocalName("integer"), None)
    ))

    val result2 = Typechecker.typecheck(mod2)
    result2 shouldBe 'left
    result2.left.get.head shouldBe TypeError("Cannot unify Int with Boolean")
  }

  it should "ensure the expressions provided to both branches of an if are compatible" in {
    val mod1 = mkModule("If", List(
      Let("integer", If(
        LiteralBoolean(true, NoName, None),
        LiteralInt(42, NoName, None),
        LiteralInt(41, NoName, None),
        NoName, None
      ), LocalName("integer"), None)
    ))

    val result1 = Typechecker.typecheck(mod1)
    result1 shouldBe 'right

    val mod2 = mkModule("If", List(
      Let("integer", If(
        LiteralBoolean(true, NoName, None),
        LiteralInt(42, NoName, None),
        LiteralDouble(41.0, NoName, None),
        NoName, None
      ), LocalName("integer"), None)
    ))

    val result2 = Typechecker.typecheck(mod2)
    result2 shouldBe 'left
    result2.left.get.head shouldBe TypeError("Cannot unify Int with Double")
  }

  it should "infer the parameter and return type of lambda expressions" in {
    val mod1 = mkModule("If", List(
      Let("lam", Lambda(List("bool"), If(
        Reference("bool", LocalName("bool"), None),
        LiteralInt(42, NoName, None),
        LiteralInt(41, NoName, None),
        NoName, None
      ), NoName, None), LocalName("lam"), None)
    ))

    val result = Typechecker.typecheck(mod1)
    result shouldBe 'right

    val mod2 = mkModule("Lambda", List(
      Let("lam", Lambda(List("a"),
        Reference("a", LocalName("a"), None), NoName, None
      ), LocalName("lam"), None)
    ))

    val result2 = Typechecker.typecheck(mod2)
    result2 shouldBe 'right
  }

  it should "infer the type of lambda application" in {
    val mod1 = mkModule("Apply", List(
      Let("lam", Lambda(List("bool"),
        If(
          Reference("bool", MemberName(List.empty, "Apply", "bool"), None),
          LiteralInt(42, NoName, None),
          LiteralInt(41, NoName, None),
          NoName, None
        ), NoName, None), MemberName(List.empty, "Apply", "lam"), None),
      Let("app", Apply(
        Reference("lam", MemberName(List.empty, "Apply", "bool"), None),
        List(LiteralBoolean(true, NoName, None)),
        NoName, None
      ), MemberName(List.empty, "Apply", "app"), None)
    ))

    val result1 = Typechecker.typecheck(mod1)
    result1 shouldBe 'right

    val mod2 = mkModule("Apply", List(
      Let("lam", Lambda(List("a"),
        Reference("a", LocalName("a"), None), NoName, None
      ), MemberName(List.empty, "Apply", "lam"), None),
      Let("app", Apply(
        Reference("lam", MemberName(List.empty, "Apply", "bool"), None),
        List(LiteralBoolean(true, NoName, None)),
        NoName, None
      ), MemberName(List.empty, "Apply", "app"), None)
    ))

    println(Printer.print(mod2).render(80))

    val result2 = Typechecker.typecheck(mod2)
    result2 shouldBe 'right
  }
}
