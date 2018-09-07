package inc.typechecker

import inc.common._
import org.scalatest._

class TypecheckerSpec extends FlatSpec with Matchers {
  def mkModule(name: String, decls: List[TopLevelDeclaration[Name]]) = Module(
    pkg = List("Test", "Typechecker"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = ModuleName(List("Test", "Typechecker"), name))

  def mkCheckedModule(name: String, decls: List[TopLevelDeclaration[NameWithType]]) = Module(
    pkg = List("Test", "Typechecker"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = NameWithType(ModuleName(List("Test", "Typechecker"), name), Type.Module))

  "Typechecker" should "typecheck let bound literals successfully" in {
    val mod = mkModule("Int", List(Let("int", LiteralInt(42, NoName), LocalName("int"))))
    val expected = mkCheckedModule("Int", List(
      Let("int", LiteralInt(42, NameWithType(NoName, Type.Int)), NameWithType(LocalName("int"), Type.Int)))
    )
    val result = Typechecker.typecheck(mod)
    result shouldBe 'right
    result.right.get shouldBe expected
  }

  it should "typecheck let bound field references successfully" in {
    val mod = mkModule("Ref", List(
      Let("int", LiteralInt(42, NoName), LocalName("int")),
      Let("int2", Reference("int", NoName), LocalName("int2"))
    ))
    val expected = mkCheckedModule("Ref", List(
      Let("int", LiteralInt(42, NameWithType(NoName, Type.Int)), NameWithType(LocalName("int"), Type.Int)),
      Let("int2", Reference("int", NameWithType(NoName, Type.Int)), NameWithType(LocalName("int2"), Type.Int))
    ))
    val result = Typechecker.typecheck(mod)
    result shouldBe 'right
    result.right.get shouldBe expected
  }

  it should "return an error when there is a reference to a field which doesn't exist" in {
    val mod = mkModule("Ref", List(
      Let("int", LiteralInt(42, NoName), LocalName("int")),
      Let("int2", Reference("int3", NoName), LocalName("int2"))
    ))
    val result = Typechecker.typecheck(mod)
    result shouldBe 'left
  }

  it should "return an error when there is a field which is defined twice" in {
    val mod = mkModule("Ref", List(
      Let("int", LiteralInt(42, NoName), LocalName("int")),
      Let("int2", Reference("int3", NoName), LocalName("int2")),
      Let("int2", Reference("int", NoName), LocalName("int2"))
    ))
    val result = Typechecker.typecheck(mod)
    result shouldBe 'left
  }

  it should "ensure the expression provided to if is a Boolean" in {
    val mod1 = mkModule("If", List(
      Let("integer", If(LiteralBoolean(true, NoName), LiteralInt(42, NoName), LiteralInt(41, NoName), NoName), LocalName("integer"))
    ))

    val result1 = Typechecker.typecheck(mod1)
    result1 shouldBe 'right

    val mod2 = mkModule("If", List(
      Let("integer", If(LiteralInt(1, NoName), LiteralInt(42, NoName), LiteralInt(41, NoName), NoName), LocalName("integer"))
    ))

    val result2 = Typechecker.typecheck(mod2)
    result2 shouldBe 'left
    result2.left.get.head shouldBe TypeError("Cannot unify Int with Boolean")
  }

  it should "ensure the expressions provided to both branches of an if are compatible" in {
    val mod1 = mkModule("If", List(
      Let("integer", If(LiteralBoolean(true, NoName), LiteralInt(42, NoName), LiteralInt(41, NoName), NoName), LocalName("integer"))
    ))

    val result1 = Typechecker.typecheck(mod1)
    result1 shouldBe 'right

    val mod2 = mkModule("If", List(
      Let("integer", If(LiteralBoolean(true, NoName), LiteralInt(42, NoName), LiteralDouble(41.0, NoName), NoName), LocalName("integer"))
    ))

    val result2 = Typechecker.typecheck(mod2)
    result2 shouldBe 'left
    result2.left.get.head shouldBe TypeError("Cannot unify Int with Double")
  }
}
