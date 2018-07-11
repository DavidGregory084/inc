package inc.parser

import inc.common._
import org.scalatest._
import org.scalatest.prop._

class ParserSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  def parseProgram(prog: String): Module[Unit] =
    Parser.parse(prog).fold(err => fail(err.head), identity)

  "Parser" should "parse literal integers" in {
    val mod = Module(Seq.empty, "Integer", Seq(Let("integer", LiteralInt(42, ()), ())), ())
    val mod2 = Module(Seq.empty, "Integer", Seq(Let("integer", LiteralInt(42, ()), ()), Let("integer2", LiteralInt(0, ()), ())), ())

    parseProgram("module Integer { let integer = 42 }") shouldBe mod

    parseProgram("module  Integer  {  let  integer  =  42  } ") shouldBe mod

    parseProgram(
      """
      |module Integer {
      |   let integer = 42
      |}
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Integer {
      |   let integer =
      |      42
      |}
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Integer
      |{
      |   let integer =
      |   {
      |      42
      |   }
      |}
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Integer {
      |   let integer =
      |
      |42
      |}
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Integer {
      |   let integer = { 42 }
      |   let integer2 = {
      |      0
      |   }
      |}
      |""".trim.stripMargin) shouldBe mod2

    parseProgram(
      """
      |module Integer { let integer = 42; let integer2 = 0 }
      |""".trim.stripMargin) shouldBe mod2

    parseProgram(
      """
      |module Integer { let integer = {42}; let integer2 = 0 }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse literal longs" in {
    val mod = Module(Seq.empty, "Long", Seq(Let("long", LiteralLong(42L, ()), ())), ())
    val mod2 = Module(Seq.empty, "Long", Seq(Let("long", LiteralLong(42L, ()), ()), Let("long2", LiteralLong(0L, ()), ())), ())

    parseProgram("module Long { let long = 42L }") shouldBe mod

    parseProgram("module  Long  {  let  long  =  42l  } ") shouldBe mod

    parseProgram(
      """
      |module Long {
      |   let long = 42L
      |}
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Long {
      |   let long =
      |      42l
      |}
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Long
      |{
      |   let long =
      |   {
      |      42l
      |   }
      |}
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Long {
      |   let long =
      |
      |42L
      |}
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Long {
      |   let long = { 42l }
      |   let long2 = {
      |      0L
      |   }
      |}
      |""".trim.stripMargin) shouldBe mod2

    parseProgram(
      """
      |module Long { let long = 42l; let long2 = 0L }
      |""".trim.stripMargin) shouldBe mod2

    parseProgram(
      """
      |module Long { let long = {42L}; let long2 = 0l }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse literal booleans" in {
    val mod = Module(Seq.empty, "Boolean", Seq(Let("boolean", LiteralBoolean(true, ()), ())), ())
    val mod2 = Module(Seq.empty, "Boolean", Seq(Let("boolean", LiteralBoolean(false, ()), ()), Let("boolean2", LiteralBoolean(true, ()), ())), ())

    parseProgram(
      """
      |module Boolean { let boolean = true }
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Boolean { let boolean = false; let boolean2 = true }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse literal floats and doubles" in {
    val mod = Module(Seq.empty, "Float", Seq(Let("float", LiteralFloat(3.142f, ()), ())), ())
    val mod2 = Module(Seq.empty, "Double", Seq(Let("double", LiteralDouble(3.142d, ()), ()), Let("double2", LiteralDouble(0.0001d, ()), ())), ())

    parseProgram(
      """
      |module Float { let float = 3.142f }
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Double { let double = 3.142; let double2 = 0.0001 }
      |""".trim.stripMargin) shouldBe mod2

    parseProgram(
      """
      |module Double { let double = 3.142d; let double2 = 0.0001D }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse references to other fields" in {
    val mod = Module(Seq.empty, "Float", Seq(
      Let("float", LiteralFloat(3.142f, ()), ()),
      Let("float2", Reference("float", ()), ())
    ), ())
    val mod2 = Module(Seq.empty, "Double", Seq(
      Let("double", LiteralDouble(3.142d, ()), ()),
      Let("double2", LiteralDouble(0.0001d, ()), ()),
      Let("double3", Reference("double2", ()), ())
    ), ())

    parseProgram(
      """
      |module Float { let float = 3.142f; let float2 = float }
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Double { let double = 3.142; let double2 = 0.0001; let double3 = double2 }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse package names" in {
    val mod = Module(Seq("Test"), "Float", Seq(Let("float", LiteralFloat(3.142f, ()), ())), ())
    val mod2 = Module(Seq("Test", "Parser"), "Double", Seq(Let("double", LiteralDouble(3.142d, ()), ()), Let("double2", LiteralDouble(0.0001d, ()), ())), ())

    parseProgram(
      """
      |module Test.Float { let float = 3.142f }
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Test.Parser.Double { let double = 3.142; let double2 = 0.0001 }
      |""".trim.stripMargin) shouldBe mod2
  }
}
