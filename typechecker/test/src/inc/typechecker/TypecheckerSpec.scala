package inc.typechecker

import inc.common._
import org.scalatest._

class TypecheckerSpec extends FlatSpec with Matchers {
  def mkModule(name: String, decls: Seq[TopLevelDeclaration[Name]]) = Module(
    pkg = Seq("Test", "Typechecker"),
    name = name,
    imports = Seq.empty,
    declarations = decls,
    meta = FullName(Seq("Test", "Typechecker"), name))

  def mkCheckedModule(name: String, decls: Seq[TopLevelDeclaration[NameWithType]]) = Module(
    pkg = Seq("Test", "Typechecker"),
    name = name,
    imports = Seq.empty,
    declarations = decls,
    meta = NameWithType(FullName(Seq("Test", "Typechecker"), name), Type.Module))

  "Typechecker" should "typecheck let bound literals successfully" in {
    val mod = mkModule("Int", Seq(Let("int", LiteralInt(42, NoName), LocalName("int"))))
    val expected = mkCheckedModule("Int", Seq(
      Let("int", LiteralInt(42, NameWithType(NoName, Type.Int)), NameWithType(LocalName("int"), Type.Int)))
    )
    val result = Typechecker.typecheck(mod)
    result shouldBe 'right
    result.right.get shouldBe expected
  }

  it should "typecheck let bound field references successfully" in {
    val mod = mkModule("Ref", Seq(
      Let("int", LiteralInt(42, NoName), LocalName("int")),
      Let("int2", Reference("int", NoName), LocalName("int2"))
    ))
    val expected = mkCheckedModule("Ref", Seq(
      Let("int", LiteralInt(42, NameWithType(NoName, Type.Int)), NameWithType(LocalName("int"), Type.Int)),
      Let("int2", Reference("int", NameWithType(NoName, Type.Int)), NameWithType(LocalName("int2"), Type.Int))
    ))
    val result = Typechecker.typecheck(mod)
    result shouldBe 'right
    result.right.get shouldBe expected
  }

  it should "return an error when there is a reference to a field which doesn't exist" in {
    val mod = mkModule("Ref", Seq(
      Let("int", LiteralInt(42, NoName), LocalName("int")),
      Let("int2", Reference("int3", NoName), LocalName("int2"))
    ))
    val result = Typechecker.typecheck(mod)
    result shouldBe 'left
  }

  it should "return an error when there is a field which is defined twice" in {
    val mod = mkModule("Ref", Seq(
      Let("int", LiteralInt(42, NoName), LocalName("int")),
      Let("int2", Reference("int3", NoName), LocalName("int2")),
      Let("int2", Reference("int", NoName), LocalName("int2"))
    ))
    val result = Typechecker.typecheck(mod)
    result shouldBe 'left
  }
}
