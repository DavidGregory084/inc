package inc.typechecker

import inc.common._
import org.scalatest._

class KindcheckerSpec extends FlatSpec with Matchers {
  val `*` = Atomic
  val `* -> *` = Parameterized(List(`*`), `*`)
  val `(* -> *) -> *` = Parameterized(List(`* -> *`), `*`)

  def mkParam(name: String, typ: Type) = {
    val scheme = TypeScheme(List.empty, typ)
    Param(name, Some(scheme), NamePosType(LocalName(name), Pos.Empty, scheme))
  }

  def mkConstr(name: String, params: List[Param[NamePosType]], parent: Data[NamePosType]) = {
    val typ = TypeScheme(parent.typeParams, Type.Function(params.map(_.meta.typ.typ), parent.meta.typ.typ))
    DataConstructor(name, params, parent.meta.typ, NamePosType(LocalName(name), Pos.Empty, typ))
  }

  def mkData(name: String, typeParams: List[TypeVariable], kind: Option[Kind] = None)(mkCases: Data[NamePosType] => List[DataConstructor[NamePosType]]) = {
    val typ =
      if (typeParams.isEmpty)
        TypeScheme(List.empty, TypeConstructor(name, kind.getOrElse(KindVariable())))
      else
        TypeScheme(typeParams, TypeApply(TypeConstructor(name, kind.getOrElse(KindVariable())), typeParams, kind.map(_ => `*`).getOrElse(KindVariable())))

    val data = Data(name, typeParams, List.empty, NamePosType(LocalName(name), Pos.Empty, typ))

    val cases = mkCases(data)

    data.copy(cases = cases)
  }

  "Kindchecker" should "infer * for a simple data type" in {
    val data = mkData("Bool", List.empty) { data =>
      List(
        mkConstr("True", List.empty, data),
        mkConstr("False", List.empty, data)
      )
    }

    val ctx = Printer.SourceContext(80, "Bool.inc", Printer.print(data).render(80))
    val checker = new Kindchecker(ctx, false)
    val actual = checker.kindcheck(data)

    val expectedData = mkData("Bool", List.empty, Some(`*`)) { data =>
      List(
        mkConstr("True", List.empty, data),
        mkConstr("False", List.empty, data)
      )
    }

    val expected = Right(expectedData.defaultKinds)

    actual shouldBe expected
  }

  it should "infer * -> * for a List-like data type" in {
    val tyVar = TypeVariable.named("A")
    val listTy = TypeApply(TypeConstructor("List", KindVariable()), List(tyVar), KindVariable())

    val data = mkData("List", List(tyVar)) { data =>
      List(
        mkConstr("Cons", List(
                   mkParam("head", tyVar),
                   mkParam("tail", listTy)
                 ), data),
        mkConstr("Nil", List.empty, data)
      )
    }

    val ctx = Printer.SourceContext(80, "List.inc", Printer.print(data).render(80))
    val checker = new Kindchecker(ctx, false)
    val actual = checker.kindcheck(data)

    val expectedTyVar = TypeVariable.named("A", kind = `*`)
    val expectedListTy = TypeApply(TypeConstructor("List", `* -> *`), List(expectedTyVar), `*`)
    val expectedData = mkData("List", List(expectedTyVar), Some(`* -> *`)) { data =>
      List(
        mkConstr("Cons", List(
                   mkParam("head", expectedTyVar),
                   mkParam("tail", expectedListTy)), data),
        mkConstr("Nil", List.empty, data))
    }

    val expected = Right(expectedData.defaultKinds)

    actual shouldBe expected
  }

  it should "infer (* -> *) -> * for a Fix-like data type" in {
    val tyVar = TypeVariable.named("F")
    val fixTy = TypeApply(TypeConstructor("Fix", KindVariable()), List(tyVar), KindVariable())
    val unfixTy = TypeApply(tyVar, List(fixTy), `*`)

    val data = mkData("Fix", List(tyVar)) { data =>
      List(mkConstr("Fix", List(mkParam("unfix", unfixTy)), data))
    }

    val ctx = Printer.SourceContext(80, "Fix.inc", Printer.print(data).render(80))
    val checker = new Kindchecker(ctx, false)
    val actual = checker.kindcheck(data)

    val expectedTyVar = TypeVariable.named("F", kind = `* -> *`)
    val expectedFixTy = TypeApply(TypeConstructor("Fix", `(* -> *) -> *`), List(expectedTyVar), `*`)

    val expectedUnfixTy = TypeApply(expectedTyVar, List(expectedFixTy), `*`)
    val expectedData = mkData("Fix", List(expectedTyVar), Some(`(* -> *) -> *`)) { data =>
      List(mkConstr("Fix", List(mkParam("unfix", expectedUnfixTy)), data))
    }

    val expected = Right(expectedData.defaultKinds)

    actual shouldBe expected
  }
}
