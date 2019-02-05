package inc.parser

import inc.common._
import org.scalatest._
import org.scalatest.prop._

class ParserSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  def parseProgram(prog: String): Module[Unit] =
    Parser.parse(prog).fold(err => fail(err.head.msg), identity)

  def mkMod(name: String, decls: List[TopLevelDeclaration[Unit]]): Module[Unit] =
    Module(List.empty, name, List.empty, decls, Pos.Empty, ())
  def mkMod(pkg: List[String], name: String, decls: List[TopLevelDeclaration[Unit]]): Module[Unit] =
    Module(pkg, name, List.empty, decls, Pos.Empty, ())
  def mkMod(pkg: List[String], name: String, imports: List[Import], decls: List[TopLevelDeclaration[Unit]]): Module[Unit] =
    Module(pkg, name, imports, decls, Pos.Empty, ())
  def mkLet(name: String, binding: Expr[Unit]): Let[Unit] =
    Let(name, binding, Pos.Empty, ())
  def mkInt(i: Int): LiteralInt[Unit] =
    LiteralInt(i, Pos.Empty, ())
  def mkLong(l: Long): LiteralLong[Unit] =
    LiteralLong(l, Pos.Empty, ())
  def mkBool(b: Boolean): LiteralBoolean[Unit] =
    LiteralBoolean(b, Pos.Empty, ())
  def mkFloat(l: Float): LiteralFloat[Unit] =
    LiteralFloat(l, Pos.Empty, ())
  def mkDouble(l: Double): LiteralDouble[Unit] =
    LiteralDouble(l, Pos.Empty, ())
  def mkRef(name: String): Reference[Unit] =
    Reference(name, Pos.Empty, ())
  def mkUnit(): LiteralUnit[Unit] =
    LiteralUnit(Pos.Empty, ())
  def mkIf(cond: Expr[Unit], thenExpr: Expr[Unit], elseExpr: Expr[Unit]): If[Unit] =
    If(cond, thenExpr, elseExpr, Pos.Empty, ())

  "Parser" should "parse literal integers" in {
    val mod = mkMod("Integer", List(mkLet("integer", mkInt(42))))
    val mod2 = mkMod("Integer", List(mkLet("integer2", mkInt(0))))

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
    val mod = mkMod("Long", List(mkLet("long", mkLong(42L))))
    val mod2 = mkMod("Long", List(mkLet("long2", mkLong(0L))))

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
    val mod = mkMod("Boolean", List(mkLet("boolean", mkBool(true))))
    val mod2 = mkMod("Boolean", List(mkLet("boolean", mkBool(false)), mkLet("boolean2", mkBool(true))))

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
    val mod = mkMod("Float", List(mkLet("float", mkFloat(3.142f))))
    val mod2 = mkMod("Double", List(mkLet("double", mkDouble(3.142d)), mkLet("double2", mkDouble(0.0001D))))

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
    val mod = mkMod("Float", List(
      mkLet("float", mkFloat(3.142f)),
      mkLet("float2", mkRef("float"))
    ))
    val mod2 = mkMod("Double", List(
      mkLet("double", mkDouble(3.142d)),
      mkLet("double2", mkDouble(0.0001d)),
      mkLet("double3", mkRef("double2"))
    ))

    parseProgram(
      """
      |module Float { let float = 3.142f; let float2 = float }
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Double { let double = 3.142; let double2 = 0.0001; let double3 = double2 }
      |""".trim.stripMargin) shouldBe mod2
  }


  it should "parse literal unit" in {
    val mod = mkMod("Unit", List(mkLet("unit", mkUnit())))
    val mod2 = mkMod("Unit", List(mkLet("unit", mkUnit()), mkLet("unit2", mkUnit())))

    parseProgram(
      """
      |module Unit { let unit = () }
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Unit { let unit = (); let unit2 = () }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse package names" in {
    val mod = mkMod(List("Test"), "Float", List(mkLet("float", mkFloat(3.142f))))
    val mod2 = mkMod(List("Test", "Parser"), "Double", List(mkLet("double", mkDouble(3.142d)), mkLet("double2", mkDouble(0.0001d))))

    parseProgram(
      """
      |module Test.Float { let float = 3.142f }
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Test.Parser.Double { let double = 3.142; let double2 = 0.0001 }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse module imports" in {
    val mod = mkMod(
      List("Test"), "Float",
      List(ImportModule(List("Test"), "Import", Pos.Empty)),
      List(mkLet("float", mkFloat(3.142f)))
    )
    val mod2 = mkMod(
      List("Test", "Parser"), "Double",
      List(ImportSymbols(List("Test"), "Import", List("foo", "bar", "Baz"), Pos.Empty)),
      List(mkLet("double", mkDouble(3.142d)), mkLet("double2", mkDouble(0.0001d)))
    )

    parseProgram(
      """
      |module Test.Float { import Test.Import; let float = 3.142f }
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Test.Parser.Double { import Test.Import.{ foo, bar, Baz }; let double = 3.142; let double2 = 0.0001 }
      |""".trim.stripMargin) shouldBe mod2
  }

  it should "parse if expressions" in {
    val mod = mkMod("Integer", List(
      mkLet("integer", mkIf(mkBool(true), mkInt(42), mkInt(41)))
    ))
    val mod2 = mkMod("Integer", List(
      mkLet("integer", mkIf(mkBool(true), mkInt(42), mkInt(41))),
      mkLet("integer2", mkIf(mkBool(true), mkInt(0), mkInt(1)))
    ))

    parseProgram("module Integer { let integer = if true then 42 else 41 }") shouldBe mod

    parseProgram("module  Integer  {  let  integer  =  if true then 42 else 41  } ") shouldBe mod

    parseProgram(
      """
      |module Integer {
      |   let integer = if true then 42 else 41
      |}
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
      """
      |module Integer {
      |   let integer =
      |     if true
      |       then 42
      |       else 41
      |}
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
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

    parseProgram(
      """
      |module Integer {
      |   let integer =
      |     if true then
      |       42
      |     else
      |       41
      |}
      |""".trim.stripMargin) shouldBe mod

    parseProgram(
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
