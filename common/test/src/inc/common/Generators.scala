package inc.common

import cats.data.StateT
import cats.implicits._
import munit.Assertions
import org.scalacheck._
import org.scalacheck.cats.implicits._

trait Generators { self: Assertions =>
  type Env = Environment[Meta.Typed]
  type Decls = List[TopLevelDeclaration[Meta.Typed]]

  val nameGen: Gen[String] =
    for {
      len <- Gen.choose(0, 5)
      first <- Gen.alphaChar
      rest <- Gen.resize(len, Gen.alphaNumStr)
      nm = (first +: rest).mkString
      if !List("module", "import", "let", "if", "then", "else", "case", "data", "match", "with").contains(nm)
    } yield nm

  // Don't generate negative numbers: the language doesn't have operators yet so no parsing of prefix negation
  val intGen: Gen[Expr[Meta.Typed]] =
    Arbitrary.arbitrary[Int]
      .filter(_ >= 0)
      .map(LiteralInt(_, Meta.Typed(NoName, TypeScheme(Type.Int), Pos.Empty)))

  val longGen: Gen[Expr[Meta.Typed]] =
    Arbitrary.arbitrary[Long]
      .filter(_ >= 0L)
      .map(LiteralLong(_, Meta.Typed(NoName, TypeScheme(Type.Long), Pos.Empty)))

  val fltGen: Gen[Expr[Meta.Typed]] =
    Arbitrary.arbitrary[Float]
      .filter(_ >= 0F)
      .map(LiteralFloat(_, Meta.Typed(NoName, TypeScheme(Type.Float), Pos.Empty)))

  val dblGen: Gen[Expr[Meta.Typed]] =
    Arbitrary.arbitrary[Double]
      .filter(_ >= 0D)
      .map(LiteralDouble(_, Meta.Typed(NoName, TypeScheme(Type.Double), Pos.Empty)))

  val boolGen: Gen[Expr[Meta.Typed]] =
    Arbitrary.arbitrary[Boolean]
      .map(LiteralBoolean(_, Meta.Typed(NoName, TypeScheme(Type.Boolean), Pos.Empty)))

  val charGen: Gen[Expr[Meta.Typed]] =
    Gen.asciiPrintableChar.filterNot { chr =>
      // Parsing char and string escapes is not implemented yet
      chr == '\n' ||
        chr == '\r' ||
        chr == '\\' ||
        chr == '\''
    }.map(LiteralChar(_, Meta.Typed(NoName, TypeScheme(Type.Char), Pos.Empty)))

  val strGen: Gen[Expr[Meta.Typed]] =
    Gen.asciiPrintableStr.filterNot { str =>
      // Parsing char and string escapes is not implemented yet
      str.contains('\n') ||
      str.contains('\r') ||
      str.contains('\\') ||
      str.contains('"')
    }.map(LiteralString(_, Meta.Typed(NoName, TypeScheme(Type.String), Pos.Empty)))

  val unitGen: Gen[Expr[Meta.Typed]] =
    Gen.const(LiteralUnit(Meta.Typed(NoName, TypeScheme(Type.Unit), Pos.Empty)))

  val literalGens: List[Gen[Expr[Meta.Typed]]] =
    List(intGen, longGen, fltGen, dblGen, boolGen, charGen, strGen, unitGen)

  def referenceGen(env: Env): Gen[Expr[Meta.Typed]] =
    Gen.oneOf(env.valueNames.values).flatMap {
      case nm @ LocalName(_) =>
        Gen.const(Reference(List.empty, nm.shortName, Meta.Typed(nm, env.types(nm.shortName), Pos.Empty)))
      case nm @ MemberName(_, _, _) =>
        Gen.const(Reference(List.empty, nm.shortName, Meta.Typed(nm, env.types(nm.shortName), Pos.Empty)))
      case nm @ ConstrName(_, _, _, _) =>
        Gen.const(Reference(List.empty, nm.shortName, Meta.Typed(nm, env.types(nm.shortName), Pos.Empty)))
      case _ =>
        Gen.fail[Expr[Meta.Typed]]
    }

  def lambdaGen(env: Env): Gen[Expr[Meta.Typed]] =
    for {
      numArgs <- Gen.choose(1, 4)

      // Don't generate duplicate variable names
      pNms <- Gen.listOfN(numArgs, nameGen).suchThat(vs => vs.distinct.length == vs.length)

      pAscs <- Gen.listOfN(numArgs, Arbitrary.arbitrary[Boolean])

      pTps <- Gen.listOfN(numArgs, Gen.oneOf(
        TypeScheme(Type.Int),
        TypeScheme(Type.Long),
        TypeScheme(Type.Float),
        TypeScheme(Type.Double),
        TypeScheme(Type.Boolean),
        TypeScheme(Type.Char),
        TypeScheme(Type.String),
        TypeScheme(Type.Unit)))

      ps = pNms.lazyZip(pAscs).lazyZip(pTps).map {
        case (nm, ascribed, tp) =>
          Param(nm, if (ascribed) Some(tp.toExpr) else None, Meta.Typed(LocalName(nm), tp, Pos.Empty))
      }

      body <- exprGen(
        // We need to replace any existing declarations that clash with our params,
        // since params can shadow top level declarations
        env ++ Environment.empty[Meta.Typed]
            .withValueNames(ps.map(p => (p.name, p.meta.name)))
            .withTypes(ps.map(p => (p.name, p.meta.typ)))
      )

      lam <- Gen.const(Lambda(ps, body, Meta.Typed(NoName, TypeScheme(Type.Function(pTps.map(_.typ), body.meta.typ.typ)), Pos.Empty)))

    } yield lam

  def genArg(tp: Type)(env: Env): Gen[Expr[Meta.Typed]] = {
    val candidateDecls = env.types.collect {
      case (nm, ts @ TypeScheme(_, `tp`)) if !Type.builtIns.contains(nm) =>
        val name = env.valueNames(nm)
        Reference(List.empty, nm, Meta.Typed(name, ts, Pos.Empty))
    }

    val litGen = tp match {
      case Type.Int => intGen
      case Type.Long => longGen
      case Type.Float => fltGen
      case Type.Double => dblGen
      case Type.Boolean => boolGen
      case Type.Char => charGen
      case Type.String => strGen
      case Type.Unit => unitGen
      case TypeConstructor(nm, _) if env.typeNames(nm).isInstanceOf[DataName] =>
        val tcName = env.typeNames(nm)
        val tcMembers = env.members(tcName)
        for {
          constrMeta <- Gen.oneOf(tcMembers).suchThat { m =>
            // Constructors can be shadowed by lambda params
            env.valueNames(m.name.shortName).isInstanceOf[ConstrName]
          }
          TypeScheme(_, Type.Function(tpArgs)) = constrMeta.typ
          args <- tpArgs.init.traverse(tp => genArg(tp)(env))
        } yield Apply(Reference(List.empty, constrMeta.name.shortName, constrMeta), args, Meta.Typed(NoName, TypeScheme(tpArgs.last), Pos.Empty))
      case _ =>
        // Can happen when a lambda param shadows a constructor;
        // we can't find its members anymore using the name from env.names
        // because it has been replaced by a LocalName.
        // Need to refactor Environment to fix this.
        fail(s"Unknown argument type $tp, $env")
    }

    if (candidateDecls.isEmpty)
      litGen
    else
      Gen.oneOf(candidateDecls)
  }

  def applyGen(fnDecls: List[Name])(env: Env): Gen[Expr[Meta.Typed]] =
    for {
      lam <- Gen.oneOf(fnDecls)

      lambdaMeta = Meta.Typed(lam, env.types(lam.shortName), Pos.Empty)

      TypeScheme(_, Type.Function(tpArgs)) = lambdaMeta.typ

      args <- tpArgs.init.traverse(tp => genArg(tp)(env))

    } yield Apply(Reference(List.empty, lam.shortName, lambdaMeta), args, Meta.Typed(NoName, TypeScheme(tpArgs.last), Pos.Empty))

  def ifGen(env: Env): Gen[Expr[Meta.Typed]] = {
    pprint.pprintln(env.types)
    val condDecls: List[Gen[Expr[Meta.Typed]]] = boolGen :: env.types.toList.collect {
      case (nm, ts @ TypeScheme(_, Type.Boolean)) if !Type.builtIns.contains(nm) =>
        Reference(List.empty, nm, Meta.Typed(env.valueNames(nm), ts, Pos.Empty))
    }.map(Gen.const)

    val condGen = Gen.oneOf(condDecls).flatMap(identity)

    for {
      condExpr <- condGen
      thenExpr <- exprGen(env)
      elseExpr <- exprGen(env).suchThat(_.meta.typ == thenExpr.meta.typ)
    } yield If(condExpr, thenExpr, elseExpr, Meta.Typed(NoName, thenExpr.meta.typ, Pos.Empty))
  }

  def ascriptionGen(env: Env): Gen[Expr[Meta.Typed]] = {
    Gen.delay(exprGen(env)).map { expr =>
      Ascription(expr, expr.meta.typ.toExpr, expr.meta)
    }
  }

  def idPatGen(expr: Expr[Meta.Typed], env: Env): Gen[MatchCase[Meta.Typed]] = {
    for {
      name <- nameGen.suchThat(!collides(env, _))

      idPatMeta = Meta.Typed(LocalName(name), expr.meta.typ, Pos.Empty)

      idPat = IdentPattern(name, idPatMeta)

      resultExpr <- Gen.delay(exprGen(env.withValueName(name, idPatMeta.name).withType(name, idPatMeta.typ)))

    } yield MatchCase(idPat, resultExpr, Meta.Typed(NoName, resultExpr.meta.typ, Pos.Empty))
  }

  def matchIdGen(env: Env): Gen[Expr[Meta.Typed]] = {
    for {
      expr <- Gen.delay(exprGen(env))

      patNum <- Gen.choose(1, 3)

      pats <- Gen.listOfN(patNum, idPatGen(expr, env)).suchThat(_.map(_.meta.typ).distinct.length == 1)

    } yield Match(expr, pats, Meta.Typed(NoName, pats.head.meta.typ, Pos.Empty))
  }

  def constrPatGen(constr: Meta.Typed, env: Env): Gen[MatchCase[Meta.Typed]] = {
    val params = env.members(constr.name)

    for {
      paramNum <- Gen.choose(1, params.length)

      chosenParams <- Gen.listOfN(paramNum, Gen.oneOf(params)).suchThat(ps => ps.length == ps.distinct.length).map { paramMetas =>
        paramMetas.map { paramMeta =>
          val Type.Function(tpArgs) = paramMeta.typ.typ
          paramMeta.copy(typ = TypeScheme(tpArgs.last))
        }
      }

      patterns = chosenParams.map { paramMeta =>
        FieldPattern(paramMeta.name.shortName, None, paramMeta)
      }

      patEnv = env
        .withValueNames(chosenParams.map(p => (p.name.shortName, p.name)))
        .withTypes(chosenParams.map(p => (p.name.shortName, p.typ)))

      alias <- nameGen.suchThat(!collides(patEnv, _))

      useAlias <- Arbitrary.arbitrary[Boolean]

      (constrAlias, constrPatName) = if (useAlias) (Some(alias), LocalName(alias)) else (None, NoName)

      TypeScheme(bound, Type.Function(tpArgs)) = constr.typ

      constrType = TypeScheme(bound, tpArgs.last)

      constrPat = ConstrPattern(
        constr.name.shortName,
        constrAlias,
        patterns,
        Meta.Typed(constrPatName, constrType, Pos.Empty))

      newEnv = if (useAlias) patEnv.withValueName(alias, constrPatName).withType(alias, constrType) else patEnv

      resultExpr <- Gen.delay(exprGen(newEnv))

    } yield MatchCase(constrPat, resultExpr, Meta.Typed(NoName, resultExpr.meta.typ, Pos.Empty))
  }

  def matchConstrGen(env: Env): Gen[Expr[Meta.Typed]] = {
    for {
      data <- Gen.oneOf(env.typeNames.values).suchThat(_.isInstanceOf[DataName])

      constrMeta <- Gen.oneOf(env.members(data))

      TypeScheme(_, Type.Function(tpArgs)) = constrMeta.typ

      args <- tpArgs.init.traverse(tp => genArg(tp)(env))

      app = Apply(Reference(List.empty, constrMeta.name.shortName, constrMeta), args, Meta.Typed(NoName, TypeScheme(tpArgs.last), Pos.Empty))

      patNum <- Gen.choose(1, 3)

      pats <- Gen.listOfN(patNum, constrPatGen(constrMeta, env)).suchThat(_.map(_.meta.typ).distinct.length == 1)

    } yield Match(app, pats, Meta.Typed(NoName, pats.head.meta.typ, Pos.Empty))
  }

  def matchGen(env: Env): Gen[Expr[Meta.Typed]] = {
    Gen.oneOf(
      matchIdGen(env),
      matchConstrGen(env))
  }

  def exprGen(env: Env): Gen[Expr[Meta.Typed]] = {
    val fnDecls = env.types.toList.collect {
      case (nm, TypeScheme(_, Type.Function(_))) =>
        env.valueNames(nm)
    }

    val applyGens =
      if (fnDecls.nonEmpty) List(applyGen(fnDecls)(env)) else List.empty

    val noRefExprGens =
      literalGens ++ applyGens :+ lambdaGen(env) :+ ifGen(env) :+ ascriptionGen(env)

    val exprGens =
      if (env.valueNames.isEmpty)
        noRefExprGens
      else
        noRefExprGens :+ referenceGen(env) :+ matchGen(env)

    Gen.oneOf(exprGens)
      .flatMap(identity)
  }

  def letGen(modName: ModuleName, env: Env) =
    for {
      // Make sure we don't generate duplicate names
      name <- nameGen.suchThat(!collides(env, _))
      expr <- exprGen(env)
    } yield Let(name, expr, Meta.Typed(MemberName(modName.pkg, modName.mod, name), expr.meta.typ, Pos.Empty))

  def constructorGen(dataName: DataName, dataType: TypeScheme, env: Env) =
    for {
      name <- nameGen.suchThat(nm => nm != dataName.name && !collides(env, nm))

      numArgs <- Gen.choose(1, 4)

      pNms <- Gen.listOfN(
        numArgs, nameGen.suchThat(nm => nm != dataName.name && !collides(env, nm))
      ).suchThat(vs =>  vs.distinct.length == vs.length)

      pTps <- Gen.listOfN(numArgs, Gen.oneOf(
                            TypeScheme(Type.Int),
                            TypeScheme(Type.Long),
                            TypeScheme(Type.Float),
                            TypeScheme(Type.Double),
                            TypeScheme(Type.Boolean),
                            TypeScheme(Type.Char),
                            TypeScheme(Type.String),
                            TypeScheme(Type.Unit),
                            dataType))

      params = pNms.lazyZip(pTps).map {
        case (nm, tp) =>
          Param(nm, Some(tp.toExpr), Meta.Typed(LocalName(nm), tp, Pos.Empty))
      }

      typeScheme = TypeScheme(List.empty, Type.Function(pTps.map(_.typ), dataType.typ))

    } yield DataConstructor(name, params, Meta.Typed(ConstrName(dataName.pkg, dataName.mod, dataName.name, name), typeScheme, Pos.Empty))

  def collides(env: Env, name: String): Boolean = {
    env.valueNames.values
      .map(_.shortName.toUpperCase).toList
      .contains(name.toUpperCase)
  }

  def dataGen(modName: ModuleName, env: Env) =
    for {
      name <- nameGen.suchThat(!collides(env, _))

      numConstrs <- Gen.choose(1, 4)

      typeScheme = TypeScheme(List.empty, TypeConstructor(name, Atomic))

      dataName = DataName(modName.pkg, modName.mod, name)

      constrs <- Gen.listOfN(numConstrs, constructorGen(dataName, typeScheme, env)).suchThat { cs =>
        // Don't generate duplicate names
        val constrNames = cs.map(_.name)
        val noDuplicates = constrNames.map(_.toUpperCase).distinct.length == cs.length
        // Don't generate recursive data types without a non-recursive case
        val hasBaseCase = cs.exists(_.params.forall(_.meta.typ != typeScheme))
        noDuplicates && hasBaseCase
      }

    } yield Data(name, List.empty, constrs, Meta.Typed(dataName, typeScheme, Pos.Empty))

  def declGen(modName: ModuleName) = {
    StateT.modifyF[Gen, (Module[Meta.Typed], Int)] {
      case (mod, remaining) =>
        val env = mod.typeEnvironment
        val declGens = List(letGen(modName, env), dataGen(modName, env))
        Gen.oneOf(declGens).flatMap { declGen =>
          declGen.map { decl =>
            val updatedMod = mod.copy(declarations = mod.declarations :+ decl)
            (updatedMod, remaining - 1)
          }
        }
    }
  }

  def declsGen(emptyMod: Module[Meta.Typed]): Gen[Module[Meta.Typed]] =
    for {
      size <- Gen.choose(0, 9)
      modName @ ModuleName(_, _) = emptyMod.meta.name
      decls <- declGen(modName).get.flatMap {
        case (mod, remaining) =>
          if (remaining < 0)
            StateT.pure[Gen, (Module[Meta.Typed], Int), Module[Meta.Typed]](mod)
          else
            declGen(modName).get.map(_._1)
      }.runA((emptyMod, size))
    } yield decls

  implicit val arbitraryModule: Arbitrary[Module[Meta.Typed]] = Arbitrary {
    for {
      pkgLen <- Gen.choose(0, 5)
      pkg <- Gen.resize(pkgLen, Gen.listOfN(pkgLen, nameGen))
      name <- nameGen
      modName = ModuleName(pkg, name)
      emptyMod = Module(pkg, name, List.empty, List.empty, Meta.Typed(modName, TypeScheme(Type.Module), Pos.Empty))
      mod <- declsGen(emptyMod)
    } yield mod
  }
}
