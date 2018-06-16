package inc.codegen

import inc.common._
import org.scalatest._
import org.scalatest.prop._

class CodegenSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "Codegen" should "generate code for literal integers" in {
    val mod = Module(Seq.empty, "Integer", Seq(Let("integer", LiteralInt(42))))
    Codegen.generate(mod).fold(
      err => fail(err),
      _ should not be empty
    )
  }
}
