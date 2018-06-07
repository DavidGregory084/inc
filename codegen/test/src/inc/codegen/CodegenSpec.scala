package inc.codegen

import inc.common._
import org.scalatest._
import org.scalatest.prop._

class CodegenSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "Codegen" should "generate code for literal integers" in {
    Codegen.generate(Module(None, "Integer", Seq(Let("integer", LiteralInt(42))))) should not be empty
  }
}
