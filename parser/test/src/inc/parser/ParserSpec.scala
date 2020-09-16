package inc.parser

import cats.implicits._
import inc.common._
import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class ParserSpec extends ScalaCheckSuite with Generators {

  def parseModule(src: String): Module[Unit] =
    Parser.parse(src).fold(err => fail(err.head.message), identity).void

  test("Parser should parse literal integers") {
    val mod = Module(List.empty, "Integer", List.empty, List(Let("integer", LiteralInt(42, ()), ())), ())
    val mod2 = Module(List.empty, "Integer", List.empty, List(Let("integer", LiteralInt(42, ()), ()), Let("integer2", LiteralInt(0, ()), ())), ())

    assertEquals(parseModule("module Integer { let integer = 42 }"), mod)

    assertEquals(parseModule("module  Integer  {  let  integer  =  42  } "), mod)

    assertEquals(
      parseModule(
        """
        |module Integer {
        |   let integer = 42
        |}
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Integer {
        |   let integer =
        |      42
        |}
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Integer
        |{
        |   let integer =
        |   {
        |      42
        |   }
        |}
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Integer {
        |   let integer =
        |
        |42
        |}
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Integer {
        |   let integer = { 42 }
        |   let integer2 = {
        |      0
        |   }
        |}
        |""".trim.stripMargin),
      mod2
    )

    assertEquals(
      parseModule(
        """
        |module Integer { let integer = 42; let integer2 = 0 }
        |""".trim.stripMargin),
      mod2
    )

    assertEquals(
      parseModule(
        """
        |module Integer { let integer = {42}; let integer2 = 0 }
        |""".trim.stripMargin),
      mod2
    )
  }

  test("Parser should parse literal longs") {
    val mod = Module(List.empty, "Long", List.empty, List(Let("long", LiteralLong(42L, ()), ())), ())
    val mod2 = Module(List.empty, "Long", List.empty, List(Let("long", LiteralLong(42L, ()), ()), Let("long2", LiteralLong(0L, ()), ())), ())

    assertEquals(parseModule("module Long { let long = 42L }"), mod)

    assertEquals(parseModule("module  Long  {  let  long  =  42l  } "), mod)

    assertEquals(
      parseModule(
        """
        |module Long {
        |   let long = 42L
        |}
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Long {
        |   let long =
        |      42l
        |}
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Long
        |{
        |   let long =
        |   {
        |      42l
        |   }
        |}
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Long {
        |   let long =
        |
        |42L
        |}
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Long {
        |   let long = { 42l }
        |   let long2 = {
        |      0L
        |   }
        |}
        |""".trim.stripMargin),
      mod2
    )

    assertEquals(
      parseModule(
        """
        |module Long { let long = 42l; let long2 = 0L }
        |""".trim.stripMargin),
      mod2
    )

    assertEquals(
      parseModule(
        """
        |module Long { let long = {42L}; let long2 = 0l }
        |""".trim.stripMargin),
      mod2
    )
  }

  test("Parser should parse literal booleans") {
    val mod = Module(List.empty, "Boolean", List.empty, List(Let("boolean", LiteralBoolean(true, ()), ())), ())
    val mod2 = Module(List.empty, "Boolean", List.empty, List(Let("boolean", LiteralBoolean(false, ()), ()), Let("boolean2", LiteralBoolean(true, ()), ())), ())

    assertEquals(
      parseModule(
        """
        |module Boolean { let boolean = true }
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Boolean { let boolean = false; let boolean2 = true }
        |""".trim.stripMargin),
      mod2
    )
  }

  test("Parser should parse literal floats and doubles") {
    val mod = Module(List.empty, "Float", List.empty, List(Let("float", LiteralFloat(3.142f, ()), ())), ())
    val mod2 = Module(List.empty, "Double", List.empty, List(Let("double", LiteralDouble(3.142d, ()), ()), Let("double2", LiteralDouble(0.0001d, ()), ())), ())

    assertEquals(
      parseModule(
        """
        |module Float { let float = 3.142f }
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Double { let double = 3.142; let double2 = 0.0001 }
        |""".trim.stripMargin),
      mod2
    )

    assertEquals(
      parseModule(
        """
        |module Double { let double = 3.142d; let double2 = 0.0001D }
        |""".trim.stripMargin),
      mod2
    )
  }

  test("Parser should parse references to other fields") {
    val mod = Module(List.empty, "Float", List.empty, List(
      Let("float", LiteralFloat(3.142f, ()), ()),
      Let("float2", Reference(List.empty, "float", ()), ())
    ), ())
    val mod2 = Module(List.empty, "Double", List.empty, List(
      Let("double", LiteralDouble(3.142d, ()), ()),
      Let("double2", LiteralDouble(0.0001d, ()), ()),
      Let("double3", Reference(List.empty, "double2", ()), ())
    ), ())

    assertEquals(
      parseModule(
        """
        |module Float { let float = 3.142f; let float2 = float }
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Double { let double = 3.142; let double2 = 0.0001; let double3 = double2 }
        |""".trim.stripMargin),
      mod2
    )
  }


  test("Parser should parse literal unit") {
    val mod = Module(List.empty, "Unit", List.empty, List(Let("unit", LiteralUnit(()), ())), ())
    val mod2 = Module(List.empty, "Unit", List.empty, List(Let("unit", LiteralUnit(()), ()), Let("unit2", LiteralUnit(()), ())), ())

    assertEquals(
      parseModule(
        """
        |module Unit { let unit = () }
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Unit { let unit = (); let unit2 = () }
        |""".trim.stripMargin),
      mod2
    )
  }

  test("Parser should parse package names") {
    val mod = Module(List("Test"), "Float", List.empty, List(Let("float", LiteralFloat(3.142f, ()), ())), ())
    val mod2 = Module(List("Test", "Parser"), "Double", List.empty, List(Let("double", LiteralDouble(3.142d, ()), ()), Let("double2", LiteralDouble(0.0001d, ()), ())), ())

    assertEquals(
      parseModule(
        """
        |module Test/Float { let float = 3.142f }
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Test/Parser/Double { let double = 3.142; let double2 = 0.0001 }
        |""".trim.stripMargin),
      mod2
    )
  }

  test("Parser should parse module imports") {
    val mod = Module(
      List("Test"), "Float",
      List(ImportModule(List("Test"), "Import", Pos(20, 38))),
      List(Let("float", LiteralFloat(3.142f, ()), ())),
      ()
    )
    val mod2 = Module(
      List("Test", "Parser"), "Double",
      List(ImportSymbols(List("Test"), "Import", List("foo", "bar", "Baz"), Pos(28, 64))),
      List(Let("double", LiteralDouble(3.142d, ()), ()), Let("double2", LiteralDouble(0.0001d, ()), ())),
      ()
    )

    assertEquals(
      parseModule(
        """
        |module Test/Float { import Test/Import; let float = 3.142f }
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Test/Parser/Double { import Test/Import.{ foo, bar, Baz }; let double = 3.142; let double2 = 0.0001 }
        |""".trim.stripMargin),
      mod2
    )
  }

  test("Parser should parse if expressions") {
    val mod = Module(List.empty, "Integer", List.empty, List(Let("integer", If(LiteralBoolean(true, ()), LiteralInt(42, ()), LiteralInt(41, ()), ()), ())), ())
    val mod2 = Module(List.empty, "Integer", List.empty, List(Let("integer", If(LiteralBoolean(true, ()), LiteralInt(42, ()), LiteralInt(41, ()), ()), ()), Let("integer2", If(LiteralBoolean(true, ()), LiteralInt(0, ()), LiteralInt(1, ()), ()), ())), ())

    assertEquals(parseModule("module Integer { let integer = if true then 42 else 41 }"), mod)

    assertEquals(parseModule("module  Integer  {  let  integer  =  if true then 42 else 41  } "), mod)

    assertEquals(
      parseModule(
        """
        |module Integer {
        |   let integer = if true then 42 else 41
        |}
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Integer {
        |   let integer =
        |     if true
        |       then 42
        |       else 41
        |}
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
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
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
      parseModule(
        """
        |module Integer {
        |   let integer =
        |     if true then
        |       42
        |     else
        |       41
        |}
        |""".trim.stripMargin),
      mod
    )

    assertEquals(
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
        |""".trim.stripMargin),
      mod2
    )
  }

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1000)

  property("Parser should round trip arbitrary modules") {
    forAll { mod: Module[Meta.Typed] =>
      val modString = Printer.print(mod, false).render(80)
      assertEquals(
        Parser.parse(modString).map(_.map(_ => Pos.Empty)),
        mod.map(_ => Pos.Empty).asRight
      )
    }
  }
}
