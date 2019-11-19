package inc.typechecker

import inc.common._
import org.scalatest._

class TypecheckerSpec extends FlatSpec with Matchers {
  def mkModule(name: String, decls: List[TopLevelDeclaration[NameWithPos]]) = Module(
    pkg = List("Test", "Typechecker"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = NameWithPos(ModuleName(List("Test", "Typechecker"), name), Pos.Empty))

  def mkLet(name: String, binding: Expr[NameWithPos]) =
    Let(name, binding, NameWithPos(LocalName(name), Pos.Empty))

  def mkInt(i: Int) = LiteralInt(i, NameWithPos(NoName, Pos.Empty))
  def mkStr(s: String) = LiteralString(s, NameWithPos(NoName, Pos.Empty))
  def mkDbl(d: Double) = LiteralDouble(d, NameWithPos(NoName, Pos.Empty))
  def mkBool(b: Boolean) = LiteralBoolean(b, NameWithPos(NoName, Pos.Empty))
  def mkRef(r: String) = Reference(r, NameWithPos(NoName, Pos.Empty))

  def mkIf(cond: Expr[NameWithPos], thenExpr: Expr[NameWithPos], elseExpr: Expr[NameWithPos]) =
    If(cond, thenExpr, elseExpr, NameWithPos(NoName, Pos.Empty))

  def mkLam(params: List[String], body: Expr[NameWithPos]) = Lambda(
    params.map(nm => Param(nm, NameWithPos(LocalName(nm), Pos.Empty))),
    body,
    NameWithPos(NoName, Pos.Empty)
  )

  def mkApp(fn: Expr[NameWithPos], args: List[Expr[NameWithPos]]) =
    Apply(fn, args, NameWithPos(NoName, Pos.Empty))

  def mkCheckedModule(name: String, decls: List[TopLevelDeclaration[NamePosType]]) = Module(
    pkg = List("Test", "Typechecker"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = NamePosType(ModuleName(List("Test", "Typechecker"), name), Pos.Empty, TypeScheme(Type.Module)))

  def mkCheckedLet(name: String, binding: Expr[NamePosType]) =
    Let(name, binding, NamePosType(LocalName(name), Pos.Empty, binding.meta.typ))

  def mkCheckedInt(i: Int) = LiteralInt(i, NamePosType(NoName, Pos.Empty, TypeScheme(Type.Int)))
  def mkCheckedRef(r: String, typ: TypeScheme) = Reference(r, NamePosType(NoName, Pos.Empty, typ))


  val noImports = Map.empty[String, TopLevelDeclaration[NameWithType]]

  "Typechecker" should "typecheck let bound literals successfully" in {
    val mod = mkModule("Int", List(mkLet("int", mkInt(42))))
    val expected = mkCheckedModule("Int", List(mkCheckedLet("int", mkCheckedInt(42))))
    Typechecker.typecheck(mod, noImports, "").fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      mod  => mod shouldBe expected
    )
  }

  it should "typecheck let bound field references successfully" in {
    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int2", mkRef("int"))
    ))
    val expected = mkCheckedModule("Ref", List(
      mkCheckedLet("int", mkCheckedInt(42)),
      mkCheckedLet("int2", mkCheckedRef("int", TypeScheme(Type.Int)))
    ))
    Typechecker.typecheck(mod, noImports, "").fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      mod  => mod shouldBe expected
    )
  }

  it should "return an error when there is a reference to a field which doesn't exist" in {
    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int2", mkRef("int3"))
    ))
    Typechecker.typecheck(mod, noImports, "").fold(
      _ => succeed,
      _ => fail("Typechecking should fail as 'int3' is not defined")
    )
  }

  it should "ensure the expression provided to if is a Boolean" in {
    val mod1 = mkModule("If", List(
      mkLet("integer", mkIf(mkBool(true), mkInt(42), mkInt(41)))
    ))

    Typechecker.typecheck(mod1, noImports, "").fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      _    => succeed
    )

    val mod2 = mkModule("If", List(
      mkLet("integer", mkIf(mkInt(1), mkInt(42), mkInt(41)))
    ))

    Typechecker.typecheck(mod2, noImports, "") shouldBe TypeError.singleton(Pos.Empty, Red("Int") + " does not unify with " + Red("Boolean"))
  }

  it should "ensure the expressions provided to both branches of an if are compatible" in {
    val mod1 = mkModule("If", List(
      mkLet("integer", mkIf(mkBool(true), mkInt(42), mkInt(41)))
    ))

    Typechecker.typecheck(mod1, noImports, "").fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      _    => succeed
    )

    val mod2 = mkModule("If", List(
      mkLet("integer", mkIf(mkBool(true), mkInt(42), mkDbl(41.0)))
    ))

    Typechecker.typecheck(mod2, noImports, "") shouldBe TypeError.singleton(Pos.Empty, Red("Int") + " does not unify with " + Red("Double")),
  }

  it should "infer the parameter and return type of lambda expressions" in {
    val mod1 = mkModule("If", List(
      mkLet("lam", mkLam(List("bool"), mkIf(mkRef("bool"), mkInt(42), mkInt(41))))
    ))

    Typechecker.typecheck(mod1, noImports, "").fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      _    => succeed
    )

    val mod2 = mkModule("Lambda", List(
      mkLet("lam", mkLam(List("a"), mkRef("a")))
    ))

    Typechecker.typecheck(mod2, noImports, "").fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      _    => succeed
    )
  }

  it should "infer the type of lambda application" in {
    val mod1 = mkModule("Apply", List(
      mkLet("lam", mkLam(List("bool"), mkIf(mkRef("bool"), mkInt(42), mkInt(41)))),
      mkLet("app", mkApp(mkRef("lam"), List(mkBool(true))))
    ))

    Typechecker.typecheck(mod1, noImports, "").fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      _    => succeed
    )

    val mod2 = mkModule("Apply", List(
      mkLet("lam", mkLam(List("a"), mkRef("a"))),
      mkLet("app", mkApp(mkRef("lam"), List(mkBool(true))))
    ))

    Typechecker.typecheck(mod2, noImports, "").fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      _    => succeed
    )
  }

  it should "allow a polymorphic function to be instantiated to different types" in {
    val constAppliedToInt = mkApp(mkRef("const"), List(mkInt(32), mkInt(43)))
    val intReplaceWithStr = mkApp(mkRef("str"), List(constAppliedToInt))
    val constAppliedToStr = mkApp(mkRef("const"), List(intReplaceWithStr, mkStr("a")))

    val mod = mkModule("Const", List(
      mkLet("const", mkLam(List("a", "b"), mkRef("a"))),
      mkLet("str", mkLam(List("a"), mkStr("a"))),
      mkLet("app", constAppliedToStr)
    ))

    Typechecker.typecheck(mod, noImports, "").fold(
      errs => fail(s"""Typechecking failed with errors ${errs.mkString(", ")}"""),
      _    => succeed
    )
  }

  it should "fail the occurs check when trying to apply a function to itself" in {
    val mod = mkModule("Occurs", List(
      mkLet("occ", mkLam(List("f"), mkApp(mkRef("f"), List(mkRef("f")))))
    ))

    Typechecker.typecheck(mod, noImports, "") shouldBe TypeError.singleton(Pos.Empty, "Attempt to construct infinite type"),
  }
}
