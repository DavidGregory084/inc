package inc.resolver

import inc.common._
import org.scalatest._

class ResolverSpec extends FlatSpec with Matchers {
  def mkModule(name: String, decls: Seq[TopLevelDeclaration[Unit]]) = Module(
    pkg = Seq("Test", "Typechecker"),
    name = name,
    declarations = decls,
    meta = ())

  def mkResolvedModule(name: String, decls: Seq[TopLevelDeclaration[Name]]) = Module(
    pkg = Seq("Test", "Typechecker"),
    name = name,
    declarations = decls,
    meta = FullName(Seq("Test", "Typechecker"), name))

  "Resolver" should "resolve names for local let bindings" in {
    val mod = mkModule("Int", Seq(Let("int", LiteralInt(42, ()), ())))
    val expected = mkResolvedModule("Int", Seq(Let("int", LiteralInt(42, NoName), LocalName("int"))))
    val result = Resolver.resolve(mod)
    result shouldBe 'right
    result.right.get shouldBe expected
  }

  it should "return an error when there is a reference to a field which doesn't exist" in {
    val mod = mkModule("Ref", Seq(
      Let("int", LiteralInt(42, ()), ()),
      Let("int2", Reference("int3", ()), ())
    ))
    val result = Resolver.resolve(mod)
    result shouldBe 'left
  }

  it should "return an error when there is a field which is defined twice" in {
    val mod = mkModule("Ref", Seq(
      Let("int", LiteralInt(42, ()), ()),
      Let("int", LiteralInt(43, ()), ())
    ))
    val result = Resolver.resolve(mod)
    result shouldBe 'left
  }
}
