package inc.resolver

import inc.common._
import org.scalatest._

class ResolverSpec extends FlatSpec with Matchers {
  def mkModule(name: String, decls: List[TopLevelDeclaration[Unit]]) = Module(
    pkg = List("Test", "Resolver"),
    name = name,
    imports = List.empty,
    declarations = decls,
    pos = Pos.Empty,
    meta = ()
  )

  def mkLet(name: String, binding: Expr[Unit]) =
    Let(name, binding, Pos.Empty, ())

  def mkInt(i: Int) = LiteralInt(i, Pos.Empty, ())
  def mkRef(r: String) = Reference(r, Pos.Empty, ())

  def mkResolvedModule(name: String, decls: List[TopLevelDeclaration[Name]]) = Module(
    pkg = List("Test", "Resolver"),
    name = name,
    imports = List.empty,
    declarations = decls,
    pos = Pos.Empty,
    meta = ModuleName(List("Test", "Resolver"), name)
  )

  def mkResolvedLet(modName: String, name: String, binding: Expr[Name]) =
    Let(name, binding, Pos.Empty, MemberName(List("Test", "Resolver"), modName, name))

  def mkResolvedInt(i: Int) = LiteralInt(i, Pos.Empty, NoName: Name)

  "Resolver" should "resolve names for local let bindings" in {
    val mod = mkModule("Int", List(mkLet("int", mkInt(42))))
    val expected = mkResolvedModule("Int", List(
      mkResolvedLet("Int", "int", mkResolvedInt(42))
    ))
    val result = Resolver.resolve(mod)
    result shouldBe 'right
    result.right.get shouldBe expected
  }

  it should "return an error when there is a reference to a field which doesn't exist" in {
    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int2", mkRef("int3"))
    ))
    val result = Resolver.resolve(mod)
    result shouldBe 'left
  }

  it should "return an error when there is a field which is defined twice" in {
    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int", mkInt(43))
    ))
    val result = Resolver.resolve(mod)
    result shouldBe 'left
  }
}
