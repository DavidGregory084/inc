package inc.typechecker

import inc.common._
import munit.FunSuite

class KindcheckerSpec extends FunSuite {
  val `*` = Atomic
  val `* -> *` = Parameterized(List(`*`), `*`)
  val `(* -> *) -> *` = Parameterized(List(`* -> *`), `*`)

  def mkParam(name: String, typ: Type) = {
    val scheme = TypeScheme(List.empty, typ)
    Param(name, Some(scheme.toExpr), Meta.Typed(LocalName(name), scheme, Pos.Empty))
  }

  def mkConstr(name: String, params: List[Param[Meta.Typed]], parent: Data[Meta.Typed]) = {
    val typ = TypeScheme(parent.meta.typ.bound, Type.Function(params.map(_.meta.typ.typ), parent.meta.typ.typ))
    DataConstructor(name, params, Meta.Typed(LocalName(name), typ, Pos.Empty))
  }

  def mkData(name: String, typeParams: List[TypeVariable], kind: Option[Kind] = None)(mkCases: Data[Meta.Typed] => List[DataConstructor[Meta.Typed]]) = {
    val typ = TypeScheme(List.empty, TypeConstructor(name, kind.getOrElse(KindVariable())))

    val data = Data(name, typeParams.map(_.toExpr), List.empty, Meta.Typed(LocalName(name), typ, Pos.Empty))

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

    val solveState = Kindchecker.kindcheck(data, Environment.empty[Meta.Typed])

    val expectedData = mkData("Bool", List.empty, Some(`*`)) { data =>
      List(
        mkConstr("True", List.empty, data),
        mkConstr("False", List.empty, data)
      )
    }

    val actual = data.substituteKinds(solveState.subst.subst).defaultKinds

    val expected = expectedData.defaultKinds

    assertEquals(actual, expected)
  }

  test("Kindchecker should infer * -> * for a List-like data type") {
    val tyVar = TypeVariable(KindVariable())
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

    val solveState = Kindchecker.kindcheck(data, Environment.empty[Meta.Typed])

    val expectedTyVar = InferredTypeVariable(tyVar.id, kind = `*`)
    val expectedListTy = TypeApply(TypeConstructor("List", `* -> *`), List(expectedTyVar), `*`)
    val expectedData = mkData("List", List(expectedTyVar), Some(`* -> *`)) { data =>
      List(
        mkConstr("Cons", List(
                   mkParam("head", expectedTyVar),
                   mkParam("tail", expectedListTy)), data),
        mkConstr("Nil", List.empty, data))
    }

    val actual = data.substituteKinds(solveState.subst.subst).defaultKinds
    val expected = expectedData.defaultKinds

    assertEquals(actual, expected)
  }

  test("Kindchecker should infer (* -> *) -> * for a Fix-like data type") {
    val tyVar = TypeVariable(KindVariable())
    val fixTy = TypeApply(TypeConstructor("Fix", KindVariable()), List(tyVar), KindVariable())
    val unfixTy = TypeApply(tyVar, List(fixTy), `*`)

    val data = mkData("Fix", List(tyVar)) { data =>
      List(mkConstr("Fix", List(mkParam("unfix", unfixTy)), data))
    }

    val solveState = Kindchecker.kindcheck(data, Environment.empty[Meta.Typed])

    val expectedTyVar = InferredTypeVariable(tyVar.id, kind = `* -> *`)
    val expectedFixTy = TypeApply(TypeConstructor("Fix", `(* -> *) -> *`), List(expectedTyVar), `*`)

    val expectedUnfixTy = TypeApply(expectedTyVar, List(expectedFixTy), `*`)
    val expectedData = mkData("Fix", List(expectedTyVar), Some(`(* -> *) -> *`)) { data =>
      List(mkConstr("Fix", List(mkParam("unfix", expectedUnfixTy)), data))
    }

    val actual = data.substituteKinds(solveState.subst.subst).defaultKinds
    val expected = expectedData.defaultKinds

    assertEquals(actual, expected)
  }

  test("Kindchecker should enable use of data types from other modules") {
    val tyVar = TypeVariable(KindVariable())
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
    val solveState = Kindchecker.kindcheck(data, env)

    val expectedTyVar = InferredTypeVariable(tyVar.id, kind = `*`)
    val expectedListTy = TypeApply(TypeConstructor("List", `* -> *`), List(expectedTyVar), `*`)
    val expectedData = mkData("NonEmptyList", List(expectedTyVar), Some(`* -> *`)) { data =>
      List(
        mkConstr("NonEmptyList", List(
          mkParam("head", expectedTyVar),
          mkParam("tail", expectedListTy)
        ), data)
      )
    }

    val actual = data.substituteKinds(solveState.subst.subst).defaultKinds
    val expected = expectedData.defaultKinds

    assertEquals(actual, expected)
  }

  test("Kindchecker should return an error when data types from other modules are applied incorrectly") {
    val tyVar = TypeVariable(KindVariable())
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
    val solveState = Kindchecker.kindcheck(data, env)

    val expectedError = TypeError.kindUnification(Pos.Empty, `*`, `* -> *`)

    assert(
      solveState.errors.toList.contains(expectedError),
      s"Kindchecker did not produce the expected error ${expectedError.message}"
    )
  }
}
