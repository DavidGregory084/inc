package inc.parser

import cats.implicits._
import inc.common._
import org.scalatest._
import org.scalatest.prop._

class ParserSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  def parseModule(src: String): Module[Unit] =
    Parser.parse(src).fold(err => fail(err.head.msg), identity).void

  "Parser" should "parse literal integers" in {
    val mod = Module(List.empty, "Integer", List.empty, List(Let("integer", LiteralInt(42, ()), ())), ())
    val mod2 = Module(List.empty, "Integer", List.empty, List(Let("integer", LiteralInt(42, ()), ()), Let("integer2", LiteralInt(0, ()), ())), ())

    parseModule("module Integer { let integer = 42 }") shouldBe mod

    parseModule("module  Integer  {  let  integer  =  42  } ") shouldBe mod

    parseModule(
      """
      |module Integer {
      |   let integer = 42
      |}
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Integer {
      |   let integer =
      |      42
      |}
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Integer
      |{
      |   let integer =
      |   {
      |      42
      |   }
      |}
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Integer {
      |   let integer =
      |
      |42
      |}
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Integer {
      |   let integer = { 42 }
      |   let integer2 = {
      |      0
      |   }
      |}
      |""".trim.stripMargin) shouldBe mod2

    parseModule(
      """
      |module Integer { let integer = 42; let integer2 = 0 }
      |""".trim.stripMargin) shouldBe mod2

    parseModule(
      """
      |module Integer { let integer = {42}; let integer2 = 0 }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse literal longs" in {
    val mod = Module(List.empty, "Long", List.empty, List(Let("long", LiteralLong(42L, ()), ())), ())
    val mod2 = Module(List.empty, "Long", List.empty, List(Let("long", LiteralLong(42L, ()), ()), Let("long2", LiteralLong(0L, ()), ())), ())

    parseModule("module Long { let long = 42L }") shouldBe mod

    parseModule("module  Long  {  let  long  =  42l  } ") shouldBe mod

    parseModule(
      """
      |module Long {
      |   let long = 42L
      |}
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Long {
      |   let long =
      |      42l
      |}
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Long
      |{
      |   let long =
      |   {
      |      42l
      |   }
      |}
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Long {
      |   let long =
      |
      |42L
      |}
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Long {
      |   let long = { 42l }
      |   let long2 = {
      |      0L
      |   }
      |}
      |""".trim.stripMargin) shouldBe mod2

    parseModule(
      """
      |module Long { let long = 42l; let long2 = 0L }
      |""".trim.stripMargin) shouldBe mod2

    parseModule(
      """
      |module Long { let long = {42L}; let long2 = 0l }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse literal booleans" in {
    val mod = Module(List.empty, "Boolean", List.empty, List(Let("boolean", LiteralBoolean(true, ()), ())), ())
    val mod2 = Module(List.empty, "Boolean", List.empty, List(Let("boolean", LiteralBoolean(false, ()), ()), Let("boolean2", LiteralBoolean(true, ()), ())), ())

    parseModule(
      """
      |module Boolean { let boolean = true }
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Boolean { let boolean = false; let boolean2 = true }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse literal floats and doubles" in {
    val mod = Module(List.empty, "Float", List.empty, List(Let("float", LiteralFloat(3.142f, ()), ())), ())
    val mod2 = Module(List.empty, "Double", List.empty, List(Let("double", LiteralDouble(3.142d, ()), ()), Let("double2", LiteralDouble(0.0001d, ()), ())), ())

    parseModule(
      """
      |module Float { let float = 3.142f }
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Double { let double = 3.142; let double2 = 0.0001 }
      |""".trim.stripMargin) shouldBe mod2

    parseModule(
      """
      |module Double { let double = 3.142d; let double2 = 0.0001D }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse references to other fields" in {
    val mod = Module(List.empty, "Float", List.empty, List(
      Let("float", LiteralFloat(3.142f, ()), ()),
      Let("float2", Reference(List("float"), ()), ())
    ), ())
    val mod2 = Module(List.empty, "Double", List.empty, List(
      Let("double", LiteralDouble(3.142d, ()), ()),
      Let("double2", LiteralDouble(0.0001d, ()), ()),
      Let("double3", Reference(List("double2"), ()), ())
    ), ())

    parseModule(
      """
      |module Float { let float = 3.142f; let float2 = float }
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Double { let double = 3.142; let double2 = 0.0001; let double3 = double2 }
      |""".trim.stripMargin) shouldBe mod2
  }


  it should "parse literal unit" in {
    val mod = Module(List.empty, "Unit", List.empty, List(Let("unit", LiteralUnit(()), ())), ())
    val mod2 = Module(List.empty, "Unit", List.empty, List(Let("unit", LiteralUnit(()), ()), Let("unit2", LiteralUnit(()), ())), ())

    parseModule(
      """
      |module Unit { let unit = () }
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Unit { let unit = (); let unit2 = () }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse package names" in {
    val mod = Module(List("Test"), "Float", List.empty, List(Let("float", LiteralFloat(3.142f, ()), ())), ())
    val mod2 = Module(List("Test", "Parser"), "Double", List.empty, List(Let("double", LiteralDouble(3.142d, ()), ()), Let("double2", LiteralDouble(0.0001d, ()), ())), ())

    parseModule(
      """
      |module Test.Float { let float = 3.142f }
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Test.Parser.Double { let double = 3.142; let double2 = 0.0001 }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse module imports" in {
    val mod = Module(
      List("Test"), "Float",
      List(ImportModule(List("Test"), "Import")),
      List(Let("float", LiteralFloat(3.142f, ()), ())),
      ()
    )
    val mod2 = Module(
      List("Test", "Parser"), "Double",
      List(ImportSymbols(List("Test"), "Import", List("foo", "bar", "Baz"))),
      List(Let("double", LiteralDouble(3.142d, ()), ()), Let("double2", LiteralDouble(0.0001d, ()), ())),
      ()
    )

    parseModule(
      """
      |module Test.Float { import Test.Import; let float = 3.142f }
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Test.Parser.Double { import Test.Import.{ foo, bar, Baz }; let double = 3.142; let double2 = 0.0001 }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse if expressions" in {
    val mod = Module(List.empty, "Integer", List.empty, List(Let("integer", If(LiteralBoolean(true, ()), LiteralInt(42, ()), LiteralInt(41, ()), ()), ())), ())
    val mod2 = Module(List.empty, "Integer", List.empty, List(Let("integer", If(LiteralBoolean(true, ()), LiteralInt(42, ()), LiteralInt(41, ()), ()), ()), Let("integer2", If(LiteralBoolean(true, ()), LiteralInt(0, ()), LiteralInt(1, ()), ()), ())), ())

    parseModule("module Integer { let integer = if true then 42 else 41 }") shouldBe mod

    parseModule("module  Integer  {  let  integer  =  if true then 42 else 41  } ") shouldBe mod

    parseModule(
      """
      |module Integer {
      |   let integer = if true then 42 else 41
      |}
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Integer {
      |   let integer =
      |     if true
      |       then 42
      |       else 41
      |}
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Integer
      |{
      |   let integer =
      |   {
      |     if true then
      |       42
      |     else 41
      |   }
      |}
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Integer {
      |   let integer =
      |     if true then
      |       42
      |     else
      |       41
      |}
      |""".trim.stripMargin) shouldBe mod

    parseModule(
      """
      |module Integer {
      |   let integer = { if true then 42 else 41 }
      |   let integer2 = {
      |      if true
      |    then 0
      |  else 1
      |   }
      |}
      |""".trim.stripMargin) shouldBe mod2
  }
}
