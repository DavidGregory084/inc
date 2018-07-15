package inc.typechecker

import inc.common._
import org.scalatest._

class TypecheckerSpec extends FlatSpec with Matchers {
  def mkModule(name: String, decls: Seq[TopLevelDeclaration[Unit]])= Module(
    pkg = Seq("Test", "Typechecker"),
    name = name,
    declarations = decls,
    meta = ())

  "Typechecker" should "typecheck let bound literals successfully" in {
    val mod = mkModule("Int", Seq(Let("int", LiteralInt(42, ()), ())))
    val result = Typechecker.typecheck(mod)
    result shouldBe 'right
  }

  it should "typecheck let bound field references successfully" in {
    val mod = mkModule("Ref", Seq(
      Let("int", LiteralInt(42, ()), ()),
      Let("int2", Reference("int", ()), ())
    ))
    val result = Typechecker.typecheck(mod)
    result shouldBe 'right
  }

  it should "return an error when there is a reference to a field which doesn't exist" in {
    val mod = mkModule("Ref", Seq(
      Let("int", LiteralInt(42, ()), ()),
      Let("int2", Reference("int3", ()), ())
    ))
    val result = Typechecker.typecheck(mod)
    result shouldBe 'left
  }

  it should "return an error when there is a field which is defined twice" in {
    val mod = mkModule("Ref", Seq(
      Let("int", LiteralInt(42, ()), ()),
      Let("int2", Reference("int3", ()), ()),
      Let("int2", Reference("int", ()), ())
    ))
    val result = Typechecker.typecheck(mod)
    result shouldBe 'left
  }
}
