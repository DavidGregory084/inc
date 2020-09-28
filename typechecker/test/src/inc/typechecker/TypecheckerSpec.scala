package inc.typechecker

import cats.syntax.functor._
import inc.common._
import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class TypecheckerSpec extends ScalaCheckSuite with Generators {
  def mkModule(name: String, decls: List[TopLevelDeclaration[Meta.Untyped]]) = Module(
    pkg = List("Test", "Typechecker"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = Meta.Untyped(ModuleName(List("Test", "Typechecker"), name), Pos.Empty))

  def mkLet(name: String, binding: Expr[Meta.Untyped]) =
    Let(name, binding, Meta.Untyped(LocalName(name), Pos.Empty))

  def mkInt(i: Int) = LiteralInt(i, Meta.Untyped(NoName, Pos.Empty))
  def mkStr(s: String) = LiteralString(s, Meta.Untyped(NoName, Pos.Empty))
  def mkDbl(d: Double) = LiteralDouble(d, Meta.Untyped(NoName, Pos.Empty))
  def mkBool(b: Boolean) = LiteralBoolean(b, Meta.Untyped(NoName, Pos.Empty))
  def mkRef(r: String) = Reference(List.empty, r, Meta.Untyped(NoName, Pos.Empty))

  def mkIf(cond: Expr[Meta.Untyped], thenExpr: Expr[Meta.Untyped], elseExpr: Expr[Meta.Untyped]) =
    If(cond, thenExpr, elseExpr, Meta.Untyped(NoName, Pos.Empty))

  def mkLam(params: List[String], body: Expr[Meta.Untyped]) = Lambda(
    params.map(nm => Param(nm, None, Meta.Untyped(LocalName(nm), Pos.Empty))),
    body,
    Meta.Untyped(NoName, Pos.Empty)
  )

  def mkApp(fn: Expr[Meta.Untyped], args: List[Expr[Meta.Untyped]]) =
    Apply(fn, args, Meta.Untyped(NoName, Pos.Empty))

  def mkCheckedModule(name: String, decls: List[TopLevelDeclaration[Meta.Typed]]) = Module(
    pkg = List("Test", "Typechecker"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = Meta.Typed(ModuleName(List("Test", "Typechecker"), name), TypeScheme(Type.Module), Pos.Empty))

  def mkCheckedLet(name: String, binding: Expr[Meta.Typed]) =
    Let(name, binding, Meta.Typed(LocalName(name), binding.meta.typ, Pos.Empty))

  def mkCheckedInt(i: Int) = LiteralInt(i, Meta.Typed(NoName, TypeScheme(Type.Int), Pos.Empty))
  def mkCheckedRef(r: String, typ: TypeScheme) = Reference(List.empty, r, Meta.Typed(NoName, typ, Pos.Empty))


  val noImports = Environment.empty[Meta.Typed]

  test("Typechecker should typecheck let bound literals successfully") {
    val mod = mkModule("Int", List(mkLet("int", mkInt(42))))
    val expected = mkCheckedModule("Int", List(mkCheckedLet("int", mkCheckedInt(42))))
    Typechecker.typecheck(mod, noImports).fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      mod  => assertEquals(mod, expected)
    )
  }

  test("Typechecker should typecheck let bound field references successfully") {
    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int2", mkRef("int"))
    ))
    val expected = mkCheckedModule("Ref", List(
      mkCheckedLet("int", mkCheckedInt(42)),
      mkCheckedLet("int2", mkCheckedRef("int", TypeScheme(Type.Int)))
    ))
    Typechecker.typecheck(mod, noImports).fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      mod  => assertEquals(mod, expected)
    )
  }

  test("Typechecker should return an error when there is a reference to a field which doesn't exist") {
    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int2", mkRef("int3"))
    ))
    Typechecker.typecheck(mod, noImports).fold(
      identity,
      _ => fail("Typechecking should fail as 'int3' is not defined")
    )
  }

  test("Typechecker should ensure the expression provided to if is a Boolean") {
    val mod1 = mkModule("If", List(
      mkLet("integer", mkIf(mkBool(true), mkInt(42), mkInt(41)))
    ))

    Typechecker.typecheck(mod1, noImports).fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      identity
    )

    val mod2 = mkModule("If", List(
      mkLet("integer", mkIf(mkInt(1), mkInt(42), mkInt(41)))
    ))

    assertEquals(
      Typechecker.typecheck(mod2, noImports),
      Left(List(TypeError.typeUnification(Pos.Empty, Type.Int, Type.Boolean)))
    )
  }

  test("Typechecker should ensure the expressions provided to both branches of an if are compatible") {
    val mod1 = mkModule("If", List(
      mkLet("integer", mkIf(mkBool(true), mkInt(42), mkInt(41)))
    ))

    Typechecker.typecheck(mod1, noImports).fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      identity
    )

    val mod2 = mkModule("If", List(
      mkLet("integer", mkIf(mkBool(true), mkInt(42), mkDbl(41.0)))
    ))

    assertEquals(
      Typechecker.typecheck(mod2, noImports),
      Left(List(TypeError.typeUnification(Pos.Empty, Type.Int, Type.Double)))
    )
  }

  test("Typechecker should infer the parameter and return type of lambda expressions") {
    val mod1 = mkModule("If", List(
      mkLet("lam", mkLam(List("bool"), mkIf(mkRef("bool"), mkInt(42), mkInt(41))))
    ))

    Typechecker.typecheck(mod1, noImports).fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      identity
    )

    val mod2 = mkModule("Lambda", List(
      mkLet("lam", mkLam(List("a"), mkRef("a")))
    ))

    Typechecker.typecheck(mod2, noImports).fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      identity
    )
  }

  test("Typechecker should infer the type of lambda application") {
    val mod1 = mkModule("Apply", List(
      mkLet("lam", mkLam(List("bool"), mkIf(mkRef("bool"), mkInt(42), mkInt(41)))),
      mkLet("app", mkApp(mkRef("lam"), List(mkBool(true))))
    ))

    Typechecker.typecheck(mod1, noImports).fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      identity
    )

    val mod2 = mkModule("Apply", List(
      mkLet("lam", mkLam(List("a"), mkRef("a"))),
      mkLet("app", mkApp(mkRef("lam"), List(mkBool(true))))
    ))

    Typechecker.typecheck(mod2, noImports).fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      identity
    )
  }

  test("Typechecker should allow a polymorphic function to be instantiated to different types") {
    val constAppliedToInt = mkApp(mkRef("const"), List(mkInt(32), mkInt(43)))
    val intReplaceWithStr = mkApp(mkRef("str"), List(constAppliedToInt))
    val constAppliedToStr = mkApp(mkRef("const"), List(intReplaceWithStr, mkStr("a")))

    val mod = mkModule("Const", List(
      mkLet("const", mkLam(List("a", "b"), mkRef("a"))),
      mkLet("str", mkLam(List("a"), mkStr("a"))),
      mkLet("app", constAppliedToStr)
    ))

    Typechecker.typecheck(mod, noImports).fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      identity
    )
  }

  test("Typechecker should fail the occurs check when trying to apply a function to itself") {
    val mod = mkModule("Occurs", List(
      mkLet("occ", mkLam(List("f"), mkApp(mkRef("f"), List(mkRef("f")))))
    ))

    Typechecker.typecheck(mod, noImports).fold(
      err => assert(err.head.isInstanceOf[TypeOccursCheck]),
      _ => fail("Typechecking should fail as this code fails the occurs check")
    )
  }

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1000)

  property("Typechecker should succeed for arbitrary well typed programs".ignore) {
    forAll { expected: Module[Meta.Typed] =>
      val erasedTypesMod = expected.map(_.forgetType)
      Typechecker.typecheck(erasedTypesMod, Environment.empty[Meta.Typed]).fold(
        errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
        _ => passed
      )
    }
  }
}
