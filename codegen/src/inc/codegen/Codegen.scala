package inc.codegen

import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.foldable._
import cats.syntax.traverse._
import cats.syntax.parallel._
import cats.instances.either._
import cats.instances.list._
import inc.common._
import inc.rts.{ Unit => IncUnit }
import java.io.{ OutputStream, PrintWriter }
import java.lang.{ IllegalStateException, Object, String, System }
import org.objectweb.asm.{ ClassReader, ClassWriter, Label, Handle, Type => AsmType }
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.commons.{ GeneratorAdapter, Method }
import org.objectweb.asm.util.{ CheckClassAdapter, TraceClassVisitor }
import scala.{ Array, Boolean, Byte, Unit, Option, Some, None, Either, StringContext }
import scala.collection.immutable.List
import scala.Predef.{ ArrowAssoc, classOf, wrapRefArray }

object Codegen extends Codegen(false)

class Codegen(verifyCodegen: Boolean) {

  def print(code: Array[Byte], os: OutputStream = System.out): Unit = {
    val reader = new ClassReader(code)
    val writer = new PrintWriter(os)
    val visitor = new TraceClassVisitor(writer)
    reader.accept(visitor, ClassReader.SKIP_DEBUG)
  }

  def readInterface(code: Array[Byte]): Generate[Module[Meta.Typed]] = {
    val reader = new ClassReader(code)
    val visitor = new Asm.InterfaceAttributeVisitor()

    reader.accept(visitor, Array(Asm.InterfaceAttributePrototype), 0)

    val buffer = Either.fromOption(
      Option(visitor.buffer),
      List(CodegenError("Unable to find inc module data in class file data"))
    )

    for {
      buf <- buffer
      protobuf <- proto.Module.validate(buf).toEither.leftMap { t =>
        List(CodegenError("Error while decoding inc module data from class file data", t))
      }
    } yield Module.fromProto(protobuf)
  }


  def verify(classFile: ClassFile): Generate[ClassFile] = {
    // We can't do this while writing the class the first time because the max stack size and
    // local variables are not calculated in time for CheckClassAdapter by the ClassWriter
    Either.catchOnly[IllegalStateException] {
      val classReader = new ClassReader(classFile.bytes)
      val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
      val checkClass = new CheckClassAdapter(classWriter, true)
      classReader.accept(checkClass, 0)
    }.as(classFile).leftFlatMap { err =>
      CodegenError.singleton(err.getMessage)
    }
  }

  def newExpr(
    classEnv: ClassEnvironment,
    expr: Expr[Meta.Typed]
  ): Generate[ClassEnvironment] = expr match {
    // Generating code for literals expressions means pushing the corresponding constants onto the stack
    case LiteralInt(i, _) =>
      classEnv.enclosingMethodWriter.push(i)
      classEnv.asRight
    case LiteralLong(l, _) =>
      classEnv.enclosingMethodWriter.push(l)
      classEnv.asRight
    case LiteralFloat(f, _) =>
      classEnv.enclosingMethodWriter.push(f)
      classEnv.asRight
    case LiteralDouble(d, _) =>
      classEnv.enclosingMethodWriter.push(d)
      classEnv.asRight
    case LiteralBoolean(b, _) =>
      classEnv.enclosingMethodWriter.push(b)
      classEnv.asRight
    case LiteralChar(c, _) =>
      classEnv.enclosingMethodWriter.push(c.toInt)
      classEnv.asRight
    case LiteralString(s, _) =>
      classEnv.enclosingMethodWriter.push(s)
      classEnv.asRight
    case LiteralUnit(_) =>
      // We use the GETSTATIC opcode to push the Unit singleton instance object onto the stack
      val unitType = AsmType.getType(classOf[IncUnit])
      classEnv.enclosingMethodWriter.getStatic(unitType, "instance", unitType)
      classEnv.asRight
    case Ascription(ascribed, _, _) =>
      newExpr(classEnv, ascribed)
    case ref @ Reference(_, _, meta) =>
      // To generate code for variable references, we need to differentiate between top level definitions,
      // e.g. MemberName, and local or nameless expressions
      meta.name match {
        case MemberName(_, _, name) =>
          if (Type.isFunction(meta.typ.typ)) {
            // Member functions are encoded as static methods
            val declaredType = classEnv.typeEnvironment.types(ref.fullName)
            // In order to load a reference to a static method, we need to construct a method reference
            val methodHandle = Asm.methodHandle(classEnv, meta.copy(typ = declaredType))
            // This allows us to push the method onto the stack as if it was a function object
            classEnv.enclosingMethodWriter.push(methodHandle)
            classEnv.asRight
          } else {
            // Normal members are simply static fields
            val modTyp = AsmType.getObjectType(Asm.moduleName(classEnv, meta.name))
            classEnv.enclosingMethodWriter.getStatic(modTyp, name, Asm.asmType(classEnv, meta.typ.typ))
            classEnv.asRight
          }

        case ConstrName(_, _, _, _) =>
          // Constructor functions are encoded as static methods
          val declaredType = classEnv.typeEnvironment.types(ref.fullName)
          // In order to load a reference to a static method, we need to construct a method reference
          val methodHandle = Asm.methodHandle(classEnv, meta.copy(typ = declaredType))
          // This allows us to push the method onto the stack as if it was a function object
          classEnv.enclosingMethodWriter.push(methodHandle)
          classEnv.asRight

        case LocalName(_) =>
          // At the moment `LocalName` refers only to function arguments.
          // If `let` is added as an expression we will also have to consider local variables here.
          classEnv.enclosingMethodWriter.loadArg(classEnv.enclosingArgs(meta.name))
          classEnv.asRight

        case other =>
          // We shouldn't see references to modules or nameless expressions here
          CodegenError.singleton(s"Reference to unexpected name in code generation: $other")
      }
    case If(condExpr, thenExpr, elseExpr, _) =>
      val trueLabel = new Label
      val falseLabel = new Label
      // Note that we propagate the class environment through each part of the
      // if expression. This ensures that we are able to generate code for lambda
      // functions in both branches of an if expression without name collisions.
      for {
        // Compile the condition
        condEnv <- newExpr(classEnv, condExpr)
        // Jump to the else expression if false
        _  = classEnv.enclosingMethodWriter.ifZCmp(GeneratorAdapter.EQ, falseLabel)
        // Otherwise fall through to the then expression
        thenEnv <- newExpr(condEnv, thenExpr)
        // Then jump over the else expression
        _ = classEnv.enclosingMethodWriter.goTo(trueLabel)
        _ = classEnv.enclosingMethodWriter.visitLabel(falseLabel)
        elseEnv <- newExpr(thenEnv, elseExpr)
        _ = classEnv.enclosingMethodWriter.visitLabel(trueLabel)
      } yield elseEnv
    case Apply(fn, argExprs, meta) =>
      fn.meta.name match {
        // Applying a member function means calling the corresponding static method
        case member @ MemberName(_, _, name) =>
          val Type.Function(fnTpArgs) = classEnv.typeEnvironment.types(member.fullName).typ

          // Note that we don't have to generate code to load the `fn` expr in this
          // case as there is no function object to load.
          // We only need to stack the arguments to the function.
          argExprs.zipWithIndex.foldM(classEnv) {
            case (cEnv, (arg, idx)) =>
              // Load each of the argument expressions
              newExpr(cEnv, arg).map { cEnv =>
                val argType = Asm.asmType(cEnv, arg.meta.typ.typ)
                // It's important to make sure to deal with boxing or unboxing of arguments
                if (arg.meta.typ.typ.isPrimitive && !fnTpArgs(idx).isPrimitive) {
                  cEnv.enclosingMethodWriter.box(argType)
                } else if (!arg.meta.typ.typ.isPrimitive && fnTpArgs(idx).isPrimitive) {
                  cEnv.enclosingMethodWriter.unbox(argType)
                }
                cEnv
              }
          }.flatMap { argEnv =>
            // Now we can call the INVOKESTATIC opcode to call the static method
            val methodTypeArgs = fnTpArgs.map(Asm.asmType(classEnv, _))
            val method = new Method(name, methodTypeArgs.last, methodTypeArgs.init.toArray)
            val modTyp = AsmType.getObjectType(Asm.moduleName(classEnv, member))
            classEnv.enclosingMethodWriter.invokeStatic(modTyp, method)
            argEnv.asRight
          }.map { cEnv =>
            val retType = Asm.asmType(cEnv, meta.typ.typ)
            val fnRetType = Asm.asmType(cEnv, fnTpArgs.last)
            // As with the arguments, we need to make sure to deal with boxing and unboxing of returned values
            if (meta.typ.typ.isPrimitive && !fnTpArgs.last.isPrimitive) {
              cEnv.enclosingMethodWriter.unbox(retType)
            } else if (!meta.typ.typ.isPrimitive && fnTpArgs.last.isPrimitive) {
              cEnv.enclosingMethodWriter.box(fnRetType)
            // The VM requires a cast here as generic types are erased to Object
            } else if (!meta.typ.typ.isPrimitive && fnTpArgs.last.isInstanceOf[TypeVariable]) {
              cEnv.enclosingMethodWriter.checkCast(retType)
            }
            cEnv
          }

        // Applying function objects means calling the `apply` interface method on the function object
        case LocalName(_) | NoName =>
          val Type.Function(fnTpArgs) = fn.meta.typ.typ

          // For function objects, we need to load the function onto the stack first,
          // then stack the arguments to the function
          newExpr(classEnv, fn).flatMap { fnEnv =>
            argExprs.foldM(fnEnv) {
              case (cEnv, arg) =>
                newExpr(cEnv, arg).map { cEnv =>
                  val argType = Asm.asmType(cEnv, arg.meta.typ.typ)
                  // For function objects, all arguments must be boxed
                  // We don't need to cast because all reference types can be passed as java.lang.Object
                  if (arg.meta.typ.typ.isPrimitive) cEnv.enclosingMethodWriter.box(argType)
                  cEnv
                }
            }.flatMap { argEnv =>
              val fnTyp = Asm.asmType(classEnv, fn.meta.typ.typ)
              val methodTypeArgs = fnTpArgs.map(Asm.boxedAsmType(classEnv, _))
              val method = new Method("apply", methodTypeArgs.last, methodTypeArgs.init.toArray)
              classEnv.enclosingMethodWriter.invokeInterface(fnTyp, method)
              argEnv.asRight
            }.map { cEnv =>
              val retType = Asm.asmType(cEnv, meta.typ.typ)

              // Returned values must be unboxed in case subsequent operations are expecting a primitive value
              if (meta.typ.typ.isPrimitive)
                classEnv.enclosingMethodWriter.unbox(retType)
              // The VM requires a cast here as generic types are erased to Object
              if (!meta.typ.typ.isPrimitive && fnTpArgs.last.isInstanceOf[TypeVariable])
                classEnv.enclosingMethodWriter.checkCast(retType)

              cEnv
            }
          }
        case other =>
          // We should not apply arguments to anything other than member functions or lambda expressions
          CodegenError.singleton(s"Reference to unexpected name in code generation: $other")
      }

    case lam @ Lambda(params, body, meta) =>
      // To compile a lambda function, we need to create a dynamic callsite using the invokedynamic opcode.
      //
      // We also need to write out a static method for the VM to call when the apply method of the function object is called.
      //
      // This callsite will be linked at runtime the first time the function is called, using the standard Java
      // boostrap method for lambda functions, java.lang.invoke.LambdaMetafactory#metafactory.
      //
      val Type.Function(tpArgs) = meta.typ.typ

      // Free variables captured in the body of the function
      val capturedVars = lam.capturedVariables.toList
      val capturedVarsIndexed = capturedVars.zipWithIndex
      val capturedVarTypes = capturedVars.map(_.meta.typ.typ)
      val capturedVarAsmTypes = capturedVarTypes.map(Asm.asmType(classEnv, _))

      // The adapted argument list for the lambda once it's lifted out as a static method.
      // This consists of the dynamic arguments to the function with the captured variables prepended to the front.
      val adaptedArgTypes = capturedVarTypes ++ tpArgs
      val adaptedArgAsmTypes = adaptedArgTypes.map(Asm.asmType(classEnv, _))

      // We must used boxed types for the interface method since inc.rts.FunctionN is generic
      val boxedAsmTypes = tpArgs.map(Asm.boxedAsmType(classEnv, _))

      val prependedParams = capturedVarsIndexed.map {
        case (arg, idx) =>
          val newName = "captured$" + idx.toString
          Param(newName, None, arg.meta.copy(name = LocalName(newName)))
      }

      val allParams = prependedParams ++ params

      // We need to replace references to the captured vars in the body of our function
      // with the new arguments that we have prepended to the function's arguments list
      val capturedVarSubstitution = capturedVarsIndexed.map {
        case (v, idx) =>
          val oldVar = v.copy(meta = v.meta.forgetPos)
          val newName = "captured$" + idx.toString
          val newMeta = v.meta.copy(name = LocalName(newName))
          val newArgReference = Reference(List.empty, newName, newMeta)
          oldVar -> newArgReference
      }.toMap

      // Append an index to the enclosing declaration name in order to generate the static method.
      // We keep track of the number of lifted lambdas in the class generation environment.
      val liftedName = classEnv.enclosingDeclName
        .map(enclosing => enclosing + "$")
        .getOrElse("") + classEnv.liftedLambdas

      val liftedMethodAsmType = AsmType.getMethodType(adaptedArgAsmTypes.last, adaptedArgAsmTypes.init: _*)
      val liftedMethodDescriptor = liftedMethodAsmType.getDescriptor

      // Get the inc.rts.FunctionN class that should be returned by the INVOKEDYNAMIC callsite
      val lambdaType = Asm.asmType(classEnv, meta.typ.typ)

      val lambdaHandle = new Handle(
        H_INVOKESTATIC,
        Asm.internalName(classEnv.enclosingMod),
        liftedName,
        liftedMethodDescriptor,
        false
      )

      val lambdaArgs = allParams.zipWithIndex.map {
        case (param, idx) => param.meta.name -> idx
      }

      val methodWriter = Asm.staticMethod(
        classEnv, liftedName, liftedMethodAsmType, null, ACC_STATIC + ACC_SYNTHETIC)

      allParams.foreach { param =>
        methodWriter.visitParameter(param.name, ACC_FINAL)
      }

      // We need to increment the number of lifted declarations
      // so that we don't have name collisions in nested expressions
      val lambdaEnv = classEnv.copy(
        enclosingArgs = lambdaArgs.toMap,
        enclosingMethodWriter = methodWriter,
        liftedLambdas = classEnv.liftedLambdas + 1)

      val objType = AsmType.getType(classOf[Object])

      for {
        // Write out the static method with the captured vars replaced in the body.
        // We need to propagate the class environment as we may have generated
        // more lambdas in nested expressions.
        classEnvWithLambda <- newExpr(lambdaEnv, body.replace(capturedVarSubstitution)).map { updatedClassEnv =>
          methodWriter.returnValue()
          methodWriter.endMethod()
          classEnv.copy(liftedLambdas = updatedClassEnv.liftedLambdas)
        }

        // Now we stack the captured variables so that they are linked by the INVOKEDYNAMIC instruction
        classEnvWithArgs <- capturedVars.foldM(classEnvWithLambda) {
          case (env, captured) =>
            newExpr(env, captured)
        }

        // Finally we call the INVOKEDYNAMIC instruction.
        // See the docs for `java.lang.invoke.LambdaMetafactory` for more details of the arguments list.
        _ = classEnv.enclosingMethodWriter.invokeDynamic(
          // Interface method name
          "apply",
          // Lambda callsite descriptor
          AsmType.getMethodDescriptor(lambdaType, capturedVarAsmTypes: _*),
          // Bootstrap method handle
          Asm.BootstrapMethodHandle,
          // Bootstrap method arguments
          AsmType.getMethodType(objType, params.as(objType): _*),           // samMethodType
          lambdaHandle,                                                     // implMethod
          AsmType.getMethodType(boxedAsmTypes.last, boxedAsmTypes.init: _*) // instantiatedMethodType
        )

      // Return the classEnv updated with the number of lifted declarations
      } yield classEnvWithArgs
  }

  def newLetDeclaration(
    initialClassEnv: ClassEnvironment,
    let: Let[Meta.Typed]
  ): Generate[Unit] = {

    val classEnv = initialClassEnv.copy(
      enclosingDeclName = Some(let.name))

    let.binding match {
      case LiteralInt(i, _) =>
        Asm.staticField(classEnv, let.name, AsmType.INT_TYPE, i)
      case LiteralLong(l, _) =>
        Asm.staticField(classEnv, let.name, AsmType.LONG_TYPE, l)
      case LiteralFloat(f, _) =>
        Asm.staticField(classEnv, let.name, AsmType.FLOAT_TYPE, f)
      case LiteralDouble(d, _) =>
        Asm.staticField(classEnv, let.name, AsmType.DOUBLE_TYPE, d)
      case LiteralBoolean(b, _) =>
        Asm.staticField(classEnv, let.name, AsmType.BOOLEAN_TYPE, b)
      case LiteralChar(c, _) =>
        Asm.staticField(classEnv, let.name, AsmType.CHAR_TYPE, c)
      case LiteralString(s, _) =>
        Asm.staticField(classEnv, let.name, AsmType.getType(classOf[String]), s)
      case LiteralUnit(_) =>
        val unitType = AsmType.getType(classOf[IncUnit])
        Asm.staticFieldFrom(classEnv, let.name, unitType, unitType, "instance")
      case Ascription(ascribed, _, _) =>
        newLetDeclaration(classEnv, let.copy(binding = ascribed))
      case ref @ Reference(_, _, meta) =>
        val modType = Asm.asmType(classEnv, classEnv.enclosingMod.meta)
        val refType = Asm.asmType(classEnv, meta)
        for {
          _ <- Asm.staticField(classEnv, let.name, refType)
          _ <- newExpr(classEnv, ref)
        } yield classEnv.enclosingMethodWriter.putStatic(modType, let.name, refType)
      case ifExpr @ If(_, _, _, meta) =>
        val modType = Asm.asmType(classEnv, classEnv.enclosingMod.meta)
        val ifType = Asm.asmType(classEnv, meta)
        for {
          _ <- Asm.staticField(classEnv, let.name, ifType, null)
          _ <- newExpr(classEnv, ifExpr)
        } yield classEnv.enclosingMethodWriter.putStatic(modType, let.name, ifType)
      case app @ Apply(_, _, meta) =>
        val modType = Asm.asmType(classEnv, classEnv.enclosingMod.meta)
        val appType = Asm.asmType(classEnv, meta)
        for {
          _ <- Asm.staticField(classEnv, let.name, appType, null)
          _ <- newExpr(classEnv, app)
        } yield classEnv.enclosingMethodWriter.putStatic(modType, let.name, appType)
      case Lambda(params, body, meta) =>
        val TypeScheme(_, TypeApply(TypeConstructor("->", _, _), tpArgs, _, _)) = meta.typ
        val argMapping = params.zipWithIndex.map { case (param, idx) => (param.meta.name, idx) }.toMap

        val methodTypeArgs = tpArgs.map(Asm.asmType(classEnv, _))
        val methodType = AsmType.getMethodType(methodTypeArgs.last, methodTypeArgs.init: _*)
        val methodSig = JavaSignature.forMethod(classEnv, let.meta.typ)
        val methodWriter = Asm.staticMethod(classEnv, let.name, methodType, methodSig)

        params.foreach { param =>
          methodWriter.visitParameter(param.name, ACC_FINAL)
        }

        val lambdaEnv = classEnv.copy(
          enclosingArgs = argMapping,
          enclosingMethodWriter = methodWriter)

        newExpr(lambdaEnv, body).as {
          methodWriter.returnValue()
          methodWriter.endMethod()
        }
    }
  }

  def newDataConstructor(
    classEnv: ClassEnvironment,
    parent: Data[Meta.Typed],
    dataWriter: ClassWriter,
    constr: DataConstructor[Meta.Typed]
  ): Generate[ClassFile] = {
    val constrWriter = Asm.constrWriter(parent, constr)
    val modInternalName = Asm.internalName(classEnv.enclosingMod)
    val dataInternalName = Asm.internalName(parent)
    val constrInternalName = Asm.internalName(constr)
    val constrClassName = constrInternalName.split("/").last

    // The Java VM spec says that we must have InnerClasses information for each immediate enclosing class
    // and each immediate member class within a given class file.
    // The references have to be in order such that enclosing classes come first.
    dataWriter.visitInnerClass(constrInternalName, dataInternalName, constr.name, ACC_STATIC + ACC_FINAL)

    constrWriter.visitInnerClass(dataInternalName, modInternalName, parent.name, ACC_STATIC + ACC_ABSTRACT)
    constrWriter.visitInnerClass(constrInternalName, dataInternalName, constr.name, ACC_STATIC + ACC_FINAL)

    // Add a field for each parameter to the constructor
    constr.params.foreach { param =>
      Asm.addDataField(classEnv, parent, constrWriter, param)
    }

    Asm.addDataConstructor(classEnv, parent, constr, constrWriter)
    Asm.addDataConstructorFunction(classEnv, parent, constr)

    ClassFile(constrClassName, constrWriter.toByteArray()).asRight
  }

  def newDataDeclaration(
    classEnv: ClassEnvironment,
    data: Data[Meta.Typed]
  ): Generate[List[ClassFile]] = {
    val modWriter = classEnv.enclosingModWriter
    val dataWriter = Asm.dataWriter(data)
    val dataInternalName = Asm.internalName(data)
    val dataClassName = dataInternalName.split("/").last
    val modInternalName = Asm.internalName(classEnv.enclosingMod)

    Asm.addDefaultConstructor(dataWriter)

    // The Java VM spec says that we must have InnerClasses information for each immediate enclosing class
    // and each immediate member class within a given class file.
    // The references have to be in order such that enclosing classes come first.
    modWriter.visitInnerClass(dataInternalName, modInternalName, data.name, ACC_STATIC + ACC_ABSTRACT)
    dataWriter.visitInnerClass(dataInternalName, modInternalName, data.name, ACC_STATIC + ACC_ABSTRACT)

    data.cases.traverse { constr =>
      newDataConstructor(classEnv, data, dataWriter, constr)
    }.map { classFiles =>
      ClassFile(dataClassName, dataWriter.toByteArray()) :: classFiles
    }
  }

  def newTopLevelDeclaration(
    classEnv: ClassEnvironment,
    declaration: TopLevelDeclaration[Meta.Typed]
  ): Generate[List[ClassFile]] =
    declaration match {
      case data @ Data(_, _, _, _) =>
        newDataDeclaration(classEnv, data)
      case let @ Let(_, _, _) =>
        newLetDeclaration(classEnv, let).as(List.empty)
    }

  def generate(mod: Module[Meta.Typed], importedEnv: Environment): Generate[List[ClassFile]] = {
    val env = importedEnv ++ mod.environment

    val classFiles: Either[List[CodegenError], List[ClassFile]] = {
      val modWriter = Asm.moduleWriter(mod)
      val staticInit = Asm.staticInitializer(modWriter)

      modWriter.visitAttribute(Asm.InterfaceAttribute(mod.toProto.toByteArray))

      val classEnv = ClassEnvironment.empty(env, mod, modWriter, staticInit)

      mod.declarations.parFlatTraverse { decl =>
        newTopLevelDeclaration(classEnv, decl)
      }.map { classFiles =>
        staticInit.returnValue()
        staticInit.endMethod()
        modWriter.visitEnd()
        ClassFile(mod.name, modWriter.toByteArray()) :: classFiles
      }
    }

    if (verifyCodegen)
      classFiles.flatMap(_.traverse(verify))
    else
      classFiles
  }
}
