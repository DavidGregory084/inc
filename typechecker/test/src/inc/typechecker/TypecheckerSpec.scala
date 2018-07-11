package inc.typechecker

import inc.common._
import org.scalatest._

class TypecheckerSpec extends FlatSpec with Matchers {
  def mkModule(name: String, decls: Seq[TopLevelDeclaration[Unit]])= Module(
    pkg = Seq("Test", "Typechecker"),
    name = name,
    declarations = decls,
    meta = ())

  "Typechecker" should "typecheck simple declarations successfully" in {
    val mod = mkModule("Int", Seq(Let("int", LiteralInt(42, ()), ())))
    val result = Typechecker.typecheck(mod)
    result shouldBe 'right
    val success = result.right.get
    success
  }
}
