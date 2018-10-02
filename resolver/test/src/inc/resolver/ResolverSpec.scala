package inc.resolver

import inc.common._
import org.scalatest._

class ResolverSpec extends FlatSpec with Matchers {
  def mkModule(name: String, decls: List[TopLevelDeclaration[Unit]]) = Module(
    pkg = List("Test", "Resolver"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = (),
    pos = None)

  def mkResolvedModule(name: String, decls: List[TopLevelDeclaration[Name]]) = Module(
    pkg = List("Test", "Resolver"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = ModuleName(List("Test", "Resolver"), name),
    pos = None)

  "Resolver" should "resolve names for local let bindings" in {
    val mod = mkModule("Int", List(Let("int", LiteralInt(42, (), None), (), None)))
    val expected = mkResolvedModule("Int", List(
      Let("int", LiteralInt(42, NoName, None), MemberName(List("Test", "Resolver"), "Int", "int"), None)
    ))
    val result = Resolver.resolve(mod)
    result shouldBe 'right
    result.right.get shouldBe expected
  }

  it should "return an error when there is a reference to a field which doesn't exist" in {
    val mod = mkModule("Ref", List(
      Let("int", LiteralInt(42, (), None), (), None),
      Let("int2", Reference("int3", (), None), (), None)
    ))
    val result = Resolver.resolve(mod)
    result shouldBe 'left
  }

  it should "return an error when there is a field which is defined twice" in {
    val mod = mkModule("Ref", List(
      Let("int", LiteralInt(42, (), None), (), None),
      Let("int", LiteralInt(43, (), None), (), None)
    ))
    val result = Resolver.resolve(mod)
    result shouldBe 'left
  }
}
