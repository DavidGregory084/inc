package inc.resolver

import inc.common._
import munit.FunSuite

class ResolverSpec extends FunSuite {
  def mkModule(name: String, decls: List[TopLevelDeclaration[Pos]]) = Module(
    pkg = List("Test", "Resolver"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = Pos.Empty)

  def mkLet(name: String, binding: Expr[Pos]) =
    Let(name, binding, Pos.Empty)

  def mkInt(i: Int) = LiteralInt(i, Pos.Empty)
  def mkRef(r: String) = Reference(List.empty, r, Pos.Empty)

  def mkResolvedModule(name: String, decls: List[TopLevelDeclaration[Meta.Untyped]]) = Module(
    pkg = List("Test", "Resolver"),
    name = name,
    imports = List.empty,
    declarations = decls,
    meta = Meta.Untyped(ModuleName(List("Test", "Resolver"), name), Pos.Empty))

  def mkResolvedLet(modName: String, name: String, binding: Expr[Meta.Untyped]) =
    Let(name, binding, Meta.Untyped(MemberName(List("Test", "Resolver"), modName, name), Pos.Empty))

  def mkResolvedInt(i: Int) = LiteralInt(i, Meta.Untyped(NoName, Pos.Empty))

  test("Resolver should resolve names for local let bindings") {
    val mod = mkModule("Int", List(mkLet("int", mkInt(42))))
    val expected = mkResolvedModule("Int", List(
      mkResolvedLet("Int", "int", mkResolvedInt(42))
    ))
    Resolver.resolve(mod).fold(
      errs => fail(s"""Name resolution failed with errors ${errs.mkString(", ")}"""),
      mod => assertEquals(mod, expected)
    )
  }

  test("Parser should return an error when there is a reference to a field which doesn't exist") {
    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int2", mkRef("int3"))
    ))
    Resolver.resolve(mod).fold(
      identity,
      _ => fail("Resolution should fail as 'int3' is not defined")
    )
  }

  test("Parser should return an error when there is a field which is defined twice") {
    val mod = mkModule("Ref", List(
      mkLet("int", mkInt(42)),
      mkLet("int", mkInt(43))
    ))
    Resolver.resolve(mod).fold(
      identity,
      _ => fail("Resolution should fail as the field 'int' is defined twice")
    )
  }
}
