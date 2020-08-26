package inc.typechecker

import inc.common._
import munit.FunSuite

class KindcheckerSpec extends FunSuite {
  val `*` = Atomic
  val `* -> *` = Parameterized(List(`*`), `*`)
  val `(* -> *) -> *` = Parameterized(List(`* -> *`), `*`)

  def mkParam(name: String, typ: Type) = {
    val scheme = TypeScheme(List.empty, typ)
    Param(name, Some(scheme), Meta.Typed(LocalName(name), scheme, Pos.Empty))
  }

  def mkConstr(name: String, params: List[Param[Meta.Typed]], parent: Data[Meta.Typed]) = {
    val typ = TypeScheme(parent.typeParams, Type.Function(params.map(_.meta.typ.typ), parent.meta.typ.typ))
    DataConstructor(name, params, parent.meta.typ, Meta.Typed(LocalName(name), typ, Pos.Empty))
  }

  def mkData(name: String, typeParams: List[TypeVariable], kind: Option[Kind] = None)(mkCases: Data[Meta.Typed] => List[DataConstructor[Meta.Typed]]) = {
    val typ =
      if (typeParams.isEmpty)
        TypeScheme(List.empty, TypeConstructor(name, kind.getOrElse(KindVariable())))
      else
        TypeScheme(typeParams, TypeApply(TypeConstructor(name, kind.getOrElse(KindVariable())), typeParams, kind.map(_ => `*`).getOrElse(KindVariable())))

    val data = Data(name, typeParams, List.empty, Meta.Typed(LocalName(name), typ, Pos.Empty))

    val cases = mkCases(data)

    data.copy(cases = cases)
  }

  test("Kindchecker should infer * for a simple data type") {
    val data = mkData("Bool", List.empty) { data =>
      List(
        mkConstr("True", List.empty, data),
        mkConstr("False", List.empty, data)
      )
    }

    val subst = Kindchecker.kindcheck(data, Environment.empty[Meta.Typed]).map(_._2)

    val expectedData = mkData("Bool", List.empty, Some(`*`)) { data =>
      List(
        mkConstr("True", List.empty, data),
        mkConstr("False", List.empty, data)
      )
    }

    val actual = subst.map { s => data.substituteKinds(s).defaultKinds }

    val expected = Right(expectedData.defaultKinds)

    assertEquals(actual, expected)
  }

  test("Kindchecker should infer * -> * for a List-like data type") {
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

    val subst = Kindchecker.kindcheck(data, Environment.empty[Meta.Typed]).map(_._2)

    val expectedTyVar = TypeVariable.named("A", kind = `*`)
    val expectedListTy = TypeApply(TypeConstructor("List", `* -> *`), List(expectedTyVar), `*`)
    val expectedData = mkData("List", List(expectedTyVar), Some(`* -> *`)) { data =>
      List(
        mkConstr("Cons", List(
                   mkParam("head", expectedTyVar),
                   mkParam("tail", expectedListTy)), data),
        mkConstr("Nil", List.empty, data))
    }

    val actual = subst.map { s => data.substituteKinds(s).defaultKinds }
    val expected = Right(expectedData.defaultKinds)

    assertEquals(actual, expected)
  }

  test("Kindchecker should infer (* -> *) -> * for a Fix-like data type") {
    val tyVar = TypeVariable.named("F")
    val fixTy = TypeApply(TypeConstructor("Fix", KindVariable()), List(tyVar), KindVariable())
    val unfixTy = TypeApply(tyVar, List(fixTy), `*`)

    val data = mkData("Fix", List(tyVar)) { data =>
      List(mkConstr("Fix", List(mkParam("unfix", unfixTy)), data))
    }

    val subst = Kindchecker.kindcheck(data, Environment.empty[Meta.Typed]).map(_._2)

    val expectedTyVar = TypeVariable.named("F", kind = `* -> *`)
    val expectedFixTy = TypeApply(TypeConstructor("Fix", `(* -> *) -> *`), List(expectedTyVar), `*`)

    val expectedUnfixTy = TypeApply(expectedTyVar, List(expectedFixTy), `*`)
    val expectedData = mkData("Fix", List(expectedTyVar), Some(`(* -> *) -> *`)) { data =>
      List(mkConstr("Fix", List(mkParam("unfix", expectedUnfixTy)), data))
    }

    val actual = subst.map { s => data.substituteKinds(s).defaultKinds }
    val expected = Right(expectedData.defaultKinds)

    assertEquals(actual, expected)
  }

  test("Kindchecker should enable use of data types from other modules") {
    val tyVar = TypeVariable.named("A")
    val listTy = TypeApply(TypeConstructor("List", KindVariable()), List(tyVar), KindVariable())

    val data = mkData("NonEmptyList", List(tyVar)) { data =>
      List(
        mkConstr("NonEmptyList", List(
          mkParam("head", tyVar),
          mkParam("tail", listTy)
        ), data)
      )
    }

    val env = Environment.empty[Meta.Typed].withKind("List", `* -> *`)
    val subst = Kindchecker.kindcheck(data, env).map(_._2)

    val expectedTyVar = TypeVariable.named("A", kind = `*`)
    val expectedListTy = TypeApply(TypeConstructor("List", `* -> *`), List(expectedTyVar), `*`)
    val expectedData = mkData("NonEmptyList", List(expectedTyVar), Some(`* -> *`)) { data =>
      List(
        mkConstr("NonEmptyList", List(
          mkParam("head", expectedTyVar),
          mkParam("tail", expectedListTy)
        ), data)
      )
    }

    val actual = subst.map { s => data.substituteKinds(s).defaultKinds }
    val expected = Right(expectedData.defaultKinds)

    assertEquals(actual, expected)
  }

  test("Kindchecker should return an error when data types from other modules are applied incorrectly") {
    val tyVar = TypeVariable.named("A")
    val listTy = TypeConstructor("List", KindVariable())

    val data = mkData("NonEmptyList", List(tyVar)) { data =>
      List(
        mkConstr("NonEmptyList", List(
          mkParam("head", tyVar),
          mkParam("tail", listTy)
        ), data)
      )
    }

    val env = Environment.empty[Meta.Typed].withKind("List", `* -> *`)
    val subst = Kindchecker.kindcheck(data, env).map(_._2)

    val actual = subst.map { s => data.substituteKinds(s).defaultKinds }
    val expected = TypeError.kindUnification(Pos.Empty, `*`, `* -> *`)

    assertEquals(actual, expected)
  }
}
