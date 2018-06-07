package inc.parser

import inc.common._
import org.scalatest._
import org.scalatest.prop._

class ParserSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "Parser" should "parse literal integers" in {
    val mod = Module(None, "Integer", Seq(Let("integer", LiteralInt(42))))
    val mod2 = Module(None, "Integer", Seq(Let("integer", LiteralInt(42)), Let("integer2", LiteralInt(0))))

    Parser.parse("module Integer { let integer = 42 }") shouldBe mod

    Parser.parse("module  Integer  {  let  integer  =  42  } ") shouldBe mod

    Parser.parse(
      """
      |module Integer {
      |   let integer = 42
      |}
      |""".trim.stripMargin) shouldBe mod

    Parser.parse(
      """
      |module Integer {
      |   let integer =
      |      42
      |}
      |""".trim.stripMargin) shouldBe mod

    Parser.parse(
      """
      |module Integer
      |{
      |   let integer =
      |   {
      |      42
      |   }
      |}
      |""".trim.stripMargin) shouldBe mod

    Parser.parse(
      """
      |module Integer {
      |   let integer =
      |
      |42
      |}
      |""".trim.stripMargin) shouldBe mod

    Parser.parse(
      """
      |module Integer {
      |   let integer = { 42 }
      |   let integer2 = {
      |      0
      |   }
      |}
      |""".trim.stripMargin) shouldBe mod2

    Parser.parse(
      """
      |module Integer { let integer = 42; let integer2 = 0 }
      |""".trim.stripMargin) shouldBe mod2

    Parser.parse(
      """
      |module Integer { let integer = {42}; let integer2 = 0 }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse literal longs" in {
    val mod = Module(None, "Long", Seq(Let("long", LiteralLong(42L))))
    val mod2 = Module(None, "Long", Seq(Let("long", LiteralLong(42L)), Let("long2", LiteralLong(0L))))

    Parser.parse("module Long { let long = 42L }") shouldBe mod

    Parser.parse("module  Long  {  let  long  =  42l  } ") shouldBe mod

    Parser.parse(
      """
      |module Long {
      |   let long = 42L
      |}
      |""".trim.stripMargin) shouldBe mod

    Parser.parse(
      """
      |module Long {
      |   let long =
      |      42l
      |}
      |""".trim.stripMargin) shouldBe mod

    Parser.parse(
      """
      |module Long
      |{
      |   let long =
      |   {
      |      42l
      |   }
      |}
      |""".trim.stripMargin) shouldBe mod

    Parser.parse(
      """
      |module Long {
      |   let long =
      |
      |42L
      |}
      |""".trim.stripMargin) shouldBe mod

    Parser.parse(
      """
      |module Long {
      |   let long = { 42l }
      |   let long2 = {
      |      0L
      |   }
      |}
      |""".trim.stripMargin) shouldBe mod2

    Parser.parse(
      """
      |module Long { let long = 42l; let long2 = 0L }
      |""".trim.stripMargin) shouldBe mod2

    Parser.parse(
      """
      |module Long { let long = {42L}; let long2 = 0l }
      |""".trim.stripMargin) shouldBe mod2
  }
}
