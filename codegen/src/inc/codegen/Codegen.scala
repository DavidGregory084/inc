package inc.codegen

import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.instances.either._
import cats.instances.list._
import inc.common._
import inc.rts.{ Unit => IncUnit }
import java.io.{ OutputStream, PrintWriter }
import java.lang.{ Class, IllegalStateException, Object, String, System, ThreadLocal }
import java.lang.invoke.{ CallSite, LambdaMetafactory, MethodType, MethodHandle, MethodHandles }
import org.objectweb.asm.{ Attribute, ByteVector, ClassReader, ClassVisitor, ClassWriter, Label, Handle, Type => AsmType }
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.signature.SignatureWriter
import org.objectweb.asm.util.{ CheckClassAdapter, TraceClassVisitor }
import org.objectweb.asm.commons.{ GeneratorAdapter, Method }
import scala.{ Array, Boolean, Byte, Char, Int, Unit, Option, None, Either, Right, StringContext }
import scala.collection.immutable.{ List, Map }
import scala.Predef.{ classOf, wrapRefArray }

case class InterfaceAttribute(buffer: Array[Byte]) extends Attribute("IncInterface") {
  override def read(classReader: ClassReader, offset: Int, length: Int, charBuffer: Array[Char], codeAttributeOffset: Int, labels: Array[Label]) = {
    var readOffset = offset
    var bytesRead = 0
    val attrBytes = new Array[Byte](length)
    while (readOffset < (offset + length)) {
      attrBytes(bytesRead) = classReader.readByte(readOffset).toByte
      readOffset += 1
      bytesRead += 1
    }
    InterfaceAttribute(attrBytes)
  }
  override def write(classWriter: ClassWriter, code: Array[Byte], codeLength: Int, maxStack: Int, maxLocals: Int): ByteVector = {
    val byteVector = new ByteVector(buffer.length)
    byteVector.putByteArray(buffer, 0, buffer.length)
    byteVector
  }
}

class InterfaceAttributeVisitor extends ClassVisitor(ASM6) {
  var buffer: Array[Byte] = _
  override def visitAttribute(attribute: Attribute) =
    if (attribute.isInstanceOf[InterfaceAttribute])
      buffer = attribute.asInstanceOf[InterfaceAttribute].buffer
}

object Codegen extends Codegen(false)

class Codegen(verifyCodegen: Boolean) {
  type ClassEnvironment = Map[String, String]

  val InterfaceAttributePrototype = InterfaceAttribute(Array.empty)

  val BootstrapMethodDescriptor = MethodType.methodType(
    // Return type
    classOf[CallSite],
    // Stacked by the VM
    classOf[MethodHandles.Lookup], // caller
    classOf[String],               // invokedName
    classOf[MethodType],           // invokedType
    // Must be provided
    classOf[MethodType],           // samMethodType
    classOf[MethodHandle],         // implMethod
    classOf[MethodType]            // instantiatedMethodType
  ).toMethodDescriptorString()

  val BootstrapMethodHandle = new Handle(
    H_INVOKESTATIC,
    AsmType.getInternalName(classOf[LambdaMetafactory]),
    "metafactory",
    BootstrapMethodDescriptor,
    false
  )

  val liftedDefns = new ThreadLocal[Int] {
    override def initialValue = 0
  }

  def print(code: Array[Byte], os: OutputStream = System.out): Unit = {
    val reader = new ClassReader(code)
    val writer = new PrintWriter(os)
    val visitor = new TraceClassVisitor(writer)
    reader.accept(visitor, ClassReader.SKIP_DEBUG)
  }

  def readInterface(code: Array[Byte]): Either[List[Error], Module[Meta.Typed]] = {
    val reader = new ClassReader(code)
    val visitor = new InterfaceAttributeVisitor()

    reader.accept(visitor, Array(InterfaceAttributePrototype), 0)

    val buffer = Either.fromOption(
      Option(visitor.buffer),
      List(CodegenError("Unable to find inc module data in class file"))
    )

    for {
      buf <- buffer
      protobuf <- proto.Module.validate(buf).toEither.leftMap { t =>
        List(CodegenError("Error while decoding inc module data from class file", t))
      }
    } yield Module.fromProto(protobuf)
  }

  def withClassWriter(
    className: String,
    access: Int = ACC_PUBLIC + ACC_SUPER,
    signature: String = null,
    superName: String = AsmType.getInternalName(classOf[Object])
  )(f: ClassWriter => Either[List[CodegenError], List[ClassFile]]): Either[List[CodegenError], List[ClassFile]] = {
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)

    classWriter.visit(V1_8, access, className, signature, superName, null)

    f(classWriter).map { classFiles =>
      classWriter.visitEnd()
      ClassFile(className.split("/").last, classWriter.toByteArray()) :: classFiles
    }
  }

  def withGeneratorAdapter[A](
    classWriter: ClassWriter,
    methodName: String,
    methodDescriptor: String,
    access: Int = ACC_STATIC,
    signature: String = null
  )(f: GeneratorAdapter => Either[List[CodegenError], A]): Either[List[CodegenError], A] = {
    val generatorAdapter =
      new GeneratorAdapter(access, new Method(methodName, methodDescriptor), signature, null, classWriter)

    f(generatorAdapter).map { a =>
      generatorAdapter.returnValue()
      generatorAdapter.endMethod()
      a
    }
  }

  def functionClass(arity: Int): Either[List[CodegenError], Class[_]] =
    arity match {
      case 0 => Right(classOf[inc.rts.Function0[_]])
      case 1 => Right(classOf[inc.rts.Function1[_, _]])
      case 2 => Right(classOf[inc.rts.Function2[_, _, _]])
      case 3 => Right(classOf[inc.rts.Function3[_, _, _, _]])
      case 4 => Right(classOf[inc.rts.Function4[_, _, _, _, _]])
      case 5 => Right(classOf[inc.rts.Function5[_, _, _, _, _, _]])
      case 6 => Right(classOf[inc.rts.Function6[_, _, _, _, _, _, _]])
      case 7 => Right(classOf[inc.rts.Function7[_, _, _, _, _, _, _, _]])
      case 8 => Right(classOf[inc.rts.Function8[_, _, _, _, _, _, _, _, _]])
      case 9 => Right(classOf[inc.rts.Function9[_, _, _, _, _, _, _, _, _, _]])
      case 10 => Right(classOf[inc.rts.Function10[_, _, _, _, _, _, _, _, _, _, _]])
      case 11 => Right(classOf[inc.rts.Function11[_, _, _, _, _, _, _, _, _, _, _, _]])
      case 12 => Right(classOf[inc.rts.Function12[_, _, _, _, _, _, _, _, _, _, _, _, _]])
      case 13 => Right(classOf[inc.rts.Function13[_, _, _, _, _, _, _, _, _, _, _, _, _, _]])
      case 14 => Right(classOf[inc.rts.Function14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
      case 15 => Right(classOf[inc.rts.Function15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
      case 16 => Right(classOf[inc.rts.Function16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
      case 17 => Right(classOf[inc.rts.Function17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
      case 18 => Right(classOf[inc.rts.Function18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
      case 19 => Right(classOf[inc.rts.Function19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
      case 20 => Right(classOf[inc.rts.Function20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
      case 21 => Right(classOf[inc.rts.Function21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
      case 22 => Right(classOf[inc.rts.Function22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
      case _ => CodegenError.singleton("Functions of arity greater than 22 are not supported")
    }

  def descriptorFor(moduleClassName: String, env: Environment, typ: Type): Either[List[CodegenError], String] = typ match {
    case TypeConstructor("Int", _, _) =>
      Right(AsmType.INT_TYPE.getDescriptor)
    case TypeConstructor("Long", _, _) =>
      Right(AsmType.LONG_TYPE.getDescriptor)
    case TypeConstructor("Float", _, _) =>
      Right(AsmType.FLOAT_TYPE.getDescriptor)
    case TypeConstructor("Double", _, _) =>
      Right(AsmType.DOUBLE_TYPE.getDescriptor)
    case TypeConstructor("Boolean", _, _) =>
      Right(AsmType.BOOLEAN_TYPE.getDescriptor)
    case TypeConstructor("Char", _, _) =>
      Right(AsmType.CHAR_TYPE.getDescriptor)
    case TypeConstructor("String", _, _) =>
      Right(AsmType.getDescriptor(classOf[String]))
    case TypeConstructor("Unit", _, _) =>
      Right(AsmType.getDescriptor(classOf[inc.rts.Unit]))
    case TypeApply(TypeConstructor("->", _, _), params, _, _) =>
      functionClass(params.length - 1).map(AsmType.getDescriptor)
    case TypeApply(typ, _, _, _) =>
      descriptorFor(moduleClassName, env, typ)
    case TypeConstructor(name, _, _) =>
      getInternalName(env.names(name)).map { fullName =>
        AsmType.getObjectType(fullName).getDescriptor
      }
    case NamedTypeVariable(_, _, _) =>
      Right(AsmType.getDescriptor(classOf[Object]))
    case InferredTypeVariable(_, _, _) =>
      Right(AsmType.getDescriptor(classOf[Object]))
  }

  def asmTypeOf(moduleClassName: String, env: Environment, typ: Type): Either[List[CodegenError], AsmType] = typ match {
    case TypeConstructor("Int", _, _) =>
      Right(AsmType.INT_TYPE)
    case TypeConstructor("Long", _, _) =>
      Right(AsmType.LONG_TYPE)
    case TypeConstructor("Float", _, _) =>
      Right(AsmType.FLOAT_TYPE)
    case TypeConstructor("Double", _, _) =>
      Right(AsmType.DOUBLE_TYPE)
    case TypeConstructor("Boolean", _, _) =>
      Right(AsmType.BOOLEAN_TYPE)
    case TypeConstructor("Char", _, _) =>
      Right(AsmType.CHAR_TYPE)
    case TypeConstructor("String", _, _) =>
      Right(AsmType.getType(classOf[String]))
    case TypeConstructor("Unit", _, _) =>
      Right(AsmType.getType(classOf[inc.rts.Unit]))
    case TypeApply(TypeConstructor("->", _, _), params, _, _) =>
      functionClass(params.length - 1).map(AsmType.getType)
    case TypeApply(typ, _, _, _) =>
      asmTypeOf(moduleClassName, env, typ)
    case TypeConstructor(name, _, _) =>
      getInternalName(env.names(name)).map { fullName =>
        AsmType.getObjectType(fullName)
      }
    case NamedTypeVariable(_, _, _) =>
      Right(AsmType.getType(classOf[Object]))
    case InferredTypeVariable(_, _, _) =>
      Right(AsmType.getType(classOf[Object]))
  }

  def boxedAsmTypeOf(moduleClassName: String, env: Environment, typ: Type): Either[List[CodegenError], AsmType] = typ match {
    case TypeConstructor("Int", _, _) =>
      Right(AsmType.getType(classOf[java.lang.Integer]))
    case TypeConstructor("Long", _, _) =>
      Right(AsmType.getType(classOf[java.lang.Long]))
    case TypeConstructor("Float", _, _) =>
      Right(AsmType.getType(classOf[java.lang.Float]))
    case TypeConstructor("Double", _, _) =>
      Right(AsmType.getType(classOf[java.lang.Double]))
    case TypeConstructor("Boolean", _, _) =>
      Right(AsmType.getType(classOf[java.lang.Boolean]))
    case TypeConstructor("Char", _, _) =>
      Right(AsmType.getType(classOf[java.lang.Character]))
    case TypeConstructor("String", _, _) =>
      Right(AsmType.getType(classOf[String]))
    case TypeConstructor("Unit", _, _) =>
      Right(AsmType.getType(classOf[inc.rts.Unit]))
    case TypeApply(TypeConstructor("->", _, _), params, _, _) =>
      functionClass(params.length - 1).map(AsmType.getType)
    case TypeApply(typ, _, _, _) =>
      boxedAsmTypeOf(moduleClassName, env, typ)
    case TypeConstructor(name, _, _) =>
      getInternalName(env.names(name)).map { fullName =>
        AsmType.getObjectType(fullName)
      }
    case NamedTypeVariable(_, _, _) =>
      Right(AsmType.getType(classOf[Object]))
    case InferredTypeVariable(_, _, _) =>
      Right(AsmType.getType(classOf[Object]))
  }


  def newStaticField[A](moduleClassWriter: ClassWriter)(fieldName: String, fieldDescriptor: String, initialValue: A): Either[List[CodegenError], Unit] =
    Right(moduleClassWriter.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, fieldName, fieldDescriptor, null, initialValue).visitEnd())

  def newStaticFieldFrom(moduleClassWriter: ClassWriter, enclosingClass: String, staticInitializer: GeneratorAdapter)(fieldName: String, fieldDescriptor: String, referencedClass: String, referencedField: String): Either[List[CodegenError], Unit] =
    Right {
      moduleClassWriter.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, fieldName, fieldDescriptor, null, null).visitEnd()
      staticInitializer.visitFieldInsn(GETSTATIC, referencedClass, referencedField, fieldDescriptor)
      staticInitializer.visitFieldInsn(PUTSTATIC, enclosingClass, fieldName, fieldDescriptor)
    }

  def newExpr(classWriter: ClassWriter, className: String, generator: GeneratorAdapter, outerName: String, arguments: Map[String, Int], env: Environment)(expr: Expr[Meta.Typed]): Either[List[CodegenError], Unit] = expr match {
    case LiteralInt(i, _) =>
      Right(generator.visitLdcInsn(i))
    case LiteralLong(l, _) =>
      Right(generator.visitLdcInsn(l))
    case LiteralFloat(f, _) =>
      Right(generator.visitLdcInsn(f))
    case LiteralDouble(d, _) =>
      Right(generator.visitLdcInsn(d))
    case LiteralBoolean(b, _) =>
      Right(generator.visitLdcInsn(b))
    case LiteralChar(c, _) =>
      Right(generator.visitLdcInsn(c))
    case LiteralString(s, _) =>
      Right(generator.visitLdcInsn(s))
    case LiteralUnit(_) =>
      val unitClass = classOf[IncUnit]
      val descriptor = AsmType.getDescriptor(unitClass)
      val internalName = AsmType.getInternalName(unitClass)
      Right(generator.visitFieldInsn(GETSTATIC, internalName, "instance", descriptor))
    case If(cond, thenExpr, elseExpr, _) =>
      val trueLabel = new Label
      val falseLabel = new Label
      for {
        _ <- newExpr(classWriter, className, generator, outerName, arguments, env)(cond)
        _ = generator.visitJumpInsn(IFEQ, falseLabel)
        _ <- newExpr(classWriter, className, generator, outerName, arguments, env)(thenExpr)
        _ = generator.visitJumpInsn(GOTO, trueLabel)
        _ = generator.visitLabel(falseLabel)
        _ <- newExpr(classWriter, className, generator, outerName, arguments, env)(elseExpr)
        _ = generator.visitLabel(trueLabel)
      } yield ()
    case Reference(mod, name, typedMeta) =>
      for {
        descriptor <- descriptorFor(className, env, typedMeta.typ.typ)
        moduleName = getClassName(typedMeta.name, className)
        _ <- typedMeta.name match {
          case MemberName(_, _, _) =>
            Right(generator.visitFieldInsn(GETSTATIC, moduleName, name, descriptor))
          case ConstrName(_, _, _, _) =>
            Right(generator.visitFieldInsn(GETSTATIC, moduleName, name, descriptor))
          case LocalName(nm) =>
            Right(generator.loadArg(arguments(nm)))
          case NoName | ModuleName(_, _) =>
            val modString = mod.mkString("/")
            val ref = modString + "." + name
            CodegenError.singleton(s"Unable to resolve reference to variable $ref")
        }
      } yield ()

    case Ascription(expr, _, _) =>
      newExpr(classWriter, className, generator, outerName, arguments, env)(expr)

    case Apply(fn, args, typedMeta) =>
      val TypeScheme(_, TypeApply(TypeConstructor("->", _, _), tpArgs, _, _)) = fn.meta.typ
      val objectType = AsmType.getType(classOf[Object])

      for {
        fnTp <- asmTypeOf(className, env, fn.meta.typ.typ)
        retTp <- asmTypeOf(className, env, typedMeta.typ.typ)

        descriptor = AsmType.getMethodDescriptor(objectType, tpArgs.init.as(objectType): _*)

        _ <- newExpr(classWriter, className, generator, outerName, arguments, env)(fn)

        _ <- args.traverse_ { arg =>
          for {
            actualArgTp <- asmTypeOf(className, env, arg.meta.typ.typ)
            _ <- newExpr(classWriter, className, generator, outerName, arguments, env)(arg)
          } yield {
            if (arg.meta.typ.typ.isPrimitive) generator.box(actualArgTp)
          }
        }

      } yield {
        generator.visitMethodInsn(INVOKEINTERFACE, fnTp.getInternalName, "apply", descriptor, true)
        if (typedMeta.typ.typ.isPrimitive) generator.unbox(retTp)
        if (!typedMeta.typ.typ.isPrimitive) generator.visitTypeInsn(CHECKCAST, retTp.getInternalName)
      }

    case lam @ Lambda(params, body, typedMeta) =>
      val TypeScheme(_, TypeApply(TypeConstructor("->", _, _), tpArgs, _, _)) = typedMeta.typ

      val capturedVars = lam.capturedVariables.toList
      val capturedVarsWithIdx = capturedVars.zipWithIndex
      val capturedVarTypes = capturedVars.map(_.meta.typ.typ)
      val adaptedArgTypes = capturedVarTypes ++ tpArgs

      // This descriptor points to the real function including adapted args
      val descriptorForFunction = adaptedArgTypes.traverse(asmTypeOf(className, env, _)).map { args =>
        AsmType.getMethodDescriptor(args.last, args.init: _*)
      }

      // This is the instantiated type for the interface method (no adapted args)
      val boxedTypeForFunction = tpArgs.traverse(boxedAsmTypeOf(className, env, _)).map { args =>
        AsmType.getMethodType(args.last, args.init: _*)
      }

      // Manufacture params so that we can prepend captured variables to the arguments list
      val prependedParams = capturedVarsWithIdx.map {
        case (v, i) =>
          val newName = "captured$" + i
          Param(newName, None, v.meta.copy(name = LocalName(newName)))
      }

      // Replace all references to the captured variables with the adapted args
      val replaceCaptured = capturedVarsWithIdx.map {
        case (v, i) =>
          val newName = "captured$" + i
          (v.copy(meta = v.meta.forgetPos), Reference(List.empty, newName, v.meta.copy(name = LocalName(newName))))
      }.toMap

      val typeForLambda = asmTypeOf(className, env, typedMeta.typ.typ)

      for {
        functionDescriptor <- descriptorForFunction
        instantiatedFunctionType <- boxedTypeForFunction

        lambdaType <- typeForLambda

        capturedArgTps <- capturedVarTypes.traverse(asmTypeOf(className, env, _))

        liftedName = outerName + "$lifted" + liftedDefns.get

        // Write out a new static method with adapted arguments
        _ <- withGeneratorAdapter(classWriter, liftedName, functionDescriptor, ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC) { innerGen =>
          val allParams = prependedParams ++ params
          allParams.foreach(p => innerGen.visitParameter(p.name, ACC_FINAL))
          newExpr(classWriter, className, innerGen, liftedName + "$inner", allParams.map(_.name).zipWithIndex.toMap, env)(body.replace(replaceCaptured))
        }

        // Stack the captured variables for invokedynamic
        _ <- capturedVars.traverse_ { v =>
          newExpr(classWriter, className, generator, outerName, arguments, env)(v)
        }

      } yield {
        val objectType = AsmType.getType(classOf[Object])

        val genericFunctionType = AsmType.getMethodType(objectType, params.as(objectType): _*)

        val lambdaHandle = new Handle(
          H_INVOKESTATIC,
          className,
          liftedName,
          functionDescriptor,
          false
        )

        liftedDefns.set(liftedDefns.get + 1)

        generator.visitInvokeDynamicInsn(
          "apply",
          // Captured args must be passed to invokedynamic
          AsmType.getMethodDescriptor(lambdaType, capturedArgTps: _*),
          BootstrapMethodHandle,
          // Bootstrap method args
          genericFunctionType,
          lambdaHandle,
          instantiatedFunctionType
        )
      }
  }

  def newConstructorFunction(
    dataClassName: String,
    dataClassWriter: ClassWriter,
    staticInitializer: GeneratorAdapter,
    constrClassName: String,
    constr: DataConstructor[Meta.Typed],
    env: Environment
  ): Either[List[CodegenError], Unit] = {
    // Get the method descriptor of the data constructor's arguments
    val TypeScheme(_, TypeApply(TypeConstructor("->", _, _), tpArgs, _, _)) = constr.meta.typ

    // This descriptor points to the real function
    val descriptorForFunction = tpArgs.traverse(asmTypeOf(dataClassName, env, _)).map { args =>
      AsmType.getMethodDescriptor(args.last, args.init: _*)
    }

    // This is the instantiated type for the interface method
    val boxedTypeForFunction = tpArgs.traverse(boxedAsmTypeOf(dataClassName, env, _)).map { args =>
      AsmType.getMethodType(args.last, args.init: _*)
    }

    val descriptorForConstructor = tpArgs.init.traverse(asmTypeOf(dataClassName, env, _)).map { args =>
      AsmType.getMethodDescriptor(AsmType.VOID_TYPE, args: _*)
    }

    val typeForLambda = asmTypeOf(dataClassName, env, constr.meta.typ.typ)

    for {
      functionDescriptor <- descriptorForFunction
      constrDescriptor <- descriptorForConstructor
      instantiatedFunctionType <- boxedTypeForFunction

      lambdaType <- typeForLambda

      liftedName = constr.name + "$lifted"

      // Write out a new static method with adapted arguments
      _ <- withGeneratorAdapter(dataClassWriter, liftedName, functionDescriptor, ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC) { constructorGen =>
        val constrAsmType = AsmType.getObjectType(constrClassName)
        constr.params.foreach { p =>
          constructorGen.visitParameter(p.name, ACC_FINAL)
        }
        constructorGen.newInstance(constrAsmType)
        constructorGen.dup()
        constr.params.zipWithIndex.foreach {
          case (_, idx) =>
            constructorGen.loadArg(idx)
        }
        constructorGen.invokeConstructor(
          constrAsmType,
          new Method("<init>", constrDescriptor)
        )
        ().asRight
      }
    } yield {
      val objectType = AsmType.getType(classOf[Object])

      val genericFunctionType = AsmType.getMethodType(objectType, constr.params.as(objectType): _*)

      val lambdaHandle = new Handle(
        H_INVOKESTATIC,
        dataClassName,
        liftedName,
        functionDescriptor,
        false
      )

      staticInitializer.visitInvokeDynamicInsn(
        "apply",
        AsmType.getMethodDescriptor(lambdaType),
        BootstrapMethodHandle,
        // Bootstrap method args
        genericFunctionType,
        lambdaHandle,
        instantiatedFunctionType
      )
    }
  }

  def newTopLevelLet(
    moduleClassName: String,
    moduleClassWriter: ClassWriter,
    staticInitializer: GeneratorAdapter,
    let: Let[Meta.Typed],
    env: Environment
  ): Either[List[CodegenError], Unit] = {
    let.binding match {
      case LiteralInt(i, _) =>
        newStaticField(moduleClassWriter)(let.name, AsmType.INT_TYPE.getDescriptor, i)
      case LiteralLong(l, _) =>
        newStaticField(moduleClassWriter)(let.name, AsmType.LONG_TYPE.getDescriptor, l)
      case LiteralFloat(f, _) =>
        newStaticField(moduleClassWriter)(let.name, AsmType.FLOAT_TYPE.getDescriptor, f)
      case LiteralDouble(d, _) =>
        newStaticField(moduleClassWriter)(let.name, AsmType.DOUBLE_TYPE.getDescriptor, d)
      case LiteralBoolean(b, _) =>
        newStaticField(moduleClassWriter)(let.name, AsmType.BOOLEAN_TYPE.getDescriptor, b)
      case LiteralChar(c, _) =>
        newStaticField(moduleClassWriter)(let.name, AsmType.CHAR_TYPE.getDescriptor, c)
      case LiteralString(s, _) =>
        newStaticField(moduleClassWriter)(let.name, AsmType.getDescriptor(classOf[String]), s)
      case LiteralUnit(_) =>
        val unitClass = classOf[IncUnit]
        val descriptor = AsmType.getDescriptor(unitClass)
        val internalName = AsmType.getInternalName(unitClass)
        newStaticFieldFrom(moduleClassWriter, moduleClassName, staticInitializer)(let.name, descriptor, internalName, "instance")
      case Ascription(expr, _, _) =>
        newTopLevelLet(moduleClassName, moduleClassWriter, staticInitializer, let.copy(binding = expr), env)
      case Reference(_, name, typedMeta) =>
        descriptorFor(moduleClassName, env, typedMeta.typ.typ).flatMap { descriptor =>
          val moduleName = getClassName(typedMeta.name, enclosingClass = moduleClassName)
          newStaticFieldFrom(moduleClassWriter, moduleClassName, staticInitializer)(let.name, descriptor, moduleName, name)
        }
      case ifExpr @ If(_, _, _, typedMeta) =>
        for {
          descriptor <- descriptorFor(moduleClassName, env, typedMeta.typ.typ)
          _ <- newStaticField(moduleClassWriter)(let.name, descriptor, null)
          _ <- newExpr(moduleClassWriter, moduleClassName, staticInitializer, let.name, Map.empty, env)(ifExpr)
        } yield staticInitializer.visitFieldInsn(PUTSTATIC, moduleClassName, let.name, descriptor)
      case lam @ Lambda(_, _, typedMeta) =>
        for {
          descriptor <- descriptorFor(moduleClassName, env, typedMeta.typ.typ)
          _ <- newStaticField(moduleClassWriter)(let.name, descriptor, null)
          _ <- newExpr(moduleClassWriter, moduleClassName, staticInitializer, let.name, Map.empty, env)(lam)
        } yield staticInitializer.visitFieldInsn(PUTSTATIC, moduleClassName, let.name, descriptor)
      case apply @ Apply(_, _, typedMeta) =>
        for {
          descriptor <- descriptorFor(moduleClassName, env, typedMeta.typ.typ)
          _ <- newStaticField(moduleClassWriter)(let.name, descriptor, null)
          _ <- newExpr(moduleClassWriter, moduleClassName, staticInitializer, let.name, Map.empty, env)(apply)
        } yield staticInitializer.visitFieldInsn(PUTSTATIC, moduleClassName, let.name, descriptor)
    }
  }

  def newDataConstructor(
    moduleClassName: String,
    dataClassName: String,
    dataClassWriter: ClassWriter,
    data: Data[Meta.Typed],
    constr: DataConstructor[Meta.Typed],
    env: Environment
  ): Either[List[CodegenError], List[ClassFile]] = {
    val constrClassName = dataClassName + "$" + constr.name

    val constrSignature = JavaSignature.forConstructorOf(dataClassName, data)

    withClassWriter(constrClassName, ACC_FINAL + ACC_SUPER, constrSignature, dataClassName) { constrClassWriter =>
      // Reference the constructor in the data declaration
      dataClassWriter.visitInnerClass(constrClassName, dataClassName, constr.name, ACC_STATIC + ACC_FINAL)

      // Reference the data declaration and the constructor in the constructor
      constrClassWriter.visitInnerClass(dataClassName, moduleClassName, data.name, ACC_STATIC + ACC_ABSTRACT)
      constrClassWriter.visitInnerClass(constrClassName, dataClassName, constr.name, ACC_STATIC + ACC_FINAL)

      // The data declaration's constructor is a nullary void method
      val dataClassConstrDescriptor = AsmType.getMethodDescriptor(AsmType.VOID_TYPE)

      // Get the method descriptor of the data constructor's arguments
      val TypeScheme(_, TypeApply(TypeConstructor("->", _, _), tpArgs, _, _)) = constr.meta.typ

      // Java constructors are always void methods, so we don't need to deal with the return type
      val constrParamTypes = tpArgs.init.traverse(asmTypeOf(moduleClassName, env, _))
      val constrConstrDescriptor = constrParamTypes.map { argTps =>
        AsmType.getMethodDescriptor(AsmType.VOID_TYPE, argTps: _*)
      }

      val constrConstrSignature = JavaSignature.forJavaConstructor(moduleClassName, env, constr)

      asmTypeOf(moduleClassName, env, constr.meta.typ.typ).flatMap { asmType =>
        // Write a field for the constructor function
        newStaticField(dataClassWriter)(constr.name, asmType.getDescriptor, null)
      }.flatMap { _ =>
        constr.params.traverse_ { param =>
          // Write a field for each constructor parameter
          val fieldSigWriter = new SignatureWriter()
          val fieldTyp = param.meta.typ.typ
          for {
            _ <- JavaSignature.writeType(moduleClassName, env, fieldSigWriter, fieldTyp)
            asmType <- asmTypeOf(moduleClassName, env, fieldTyp)
          } yield constrClassWriter.visitField(
            ACC_PUBLIC + ACC_FINAL,
            param.name,
            asmType.getDescriptor,
            fieldSigWriter.toString(),
            null
          )
        }.flatMap { _ =>
          // Generate a Java constructor for the data constructor class
          (constrConstrDescriptor, constrConstrSignature).tupled.flatMap {
            case (desc, sig) =>
              withGeneratorAdapter(constrClassWriter, "<init>", desc, 0, sig) { gen =>

                // Invoke the superclass constructor
                gen.loadThis()
                gen.invokeConstructor(
                  AsmType.getObjectType(dataClassName),
                  new Method("<init>", dataClassConstrDescriptor)
                )

                // Load each constructor parameter into its corresponding field
                constr.params.zipWithIndex.traverse_ {
                  case (param, idx) =>
                    asmTypeOf(moduleClassName, env, param.meta.typ.typ).map { paramTyp =>
                      gen.loadThis()
                      gen.loadArg(idx)
                      gen.putField(AsmType.getObjectType(constrClassName), param.name, paramTyp)
                    }
                }.as(List.empty)
              }
          }
        }
      }
    }
  }

  def newDataDeclaration(
    moduleClassName: String,
    moduleClassWriter: ClassWriter,
    data: Data[Meta.Typed],
    env: Environment
  ): Either[List[CodegenError], List[ClassFile]] = {
    val dataClassName = moduleClassName + "$" + data.name
    val dataSignature = JavaSignature.forDataDeclaration(data)

    withClassWriter(dataClassName, ACC_SUPER + ACC_ABSTRACT, dataSignature) { dataClassWriter =>
      // The Java VM spec says that we must have InnerClasses information for each immediate enclosing class
      // and each immediate member class within a given class file.
      // The references have to be in order such that enclosing classes come first.

      // Reference the data declaration in the module
      moduleClassWriter.visitInnerClass(dataClassName, moduleClassName, data.name, ACC_STATIC + ACC_ABSTRACT)
      // Reference the module in the data declaration
      dataClassWriter.visitInnerClass(dataClassName, moduleClassName, data.name, ACC_STATIC + ACC_ABSTRACT)

      addDefaultConstructor(dataClassWriter).flatMap { _ =>
        withGeneratorAdapter(dataClassWriter, "<clinit>", AsmType.getMethodDescriptor(AsmType.VOID_TYPE)) { staticInitializer =>
          data.cases.flatTraverse { constr =>
            val constrClassName = dataClassName + "$" + constr.name
            for {
              classFiles <- newDataConstructor(moduleClassName, dataClassName, dataClassWriter, data, constr, env)
              _ <- newConstructorFunction(dataClassName, dataClassWriter, staticInitializer, constrClassName, constr, env)
            } yield classFiles
          }
        }
      }
    }
  }

  def addDefaultConstructor(classWriter: ClassWriter): Either[List[CodegenError], Unit] = {
    val objectClass = classOf[java.lang.Object]
    val objectType = AsmType.getType(objectClass)

    // The java.lang.Object constructor is a nullary void method
    val objectConstructorDescriptor = AsmType.getMethodDescriptor(AsmType.VOID_TYPE)

    withGeneratorAdapter(classWriter, "<init>", objectConstructorDescriptor, 0) { gen =>
      gen.loadThis()
      gen.invokeConstructor(objectType, new Method("<init>", objectConstructorDescriptor))
      ().asRight
    }
  }

  def newTopLevelDeclaration(
    moduleClassName: String,
    moduleClassWriter: ClassWriter,
    staticInitializer: GeneratorAdapter,
    decl: TopLevelDeclaration[Meta.Typed],
    env: Environment
  ): Either[List[CodegenError], List[ClassFile]] =
    decl match {
      case let @ Let(_, _, _) =>
        newTopLevelLet(moduleClassName, moduleClassWriter, staticInitializer, let, env).map(_ => List.empty)
      case data @ Data(_, _, _, _) =>
        newDataDeclaration(moduleClassName, moduleClassWriter, data, env)
    }

  def getClassName(name: Name, enclosingClass: String) = name match {
    case NoName =>
      enclosingClass
    case LocalName(_) =>
      enclosingClass
    case ModuleName(pkg, declaringClass) =>
      val pkgName = if (pkg.isEmpty) "" else pkg.mkString("/") + "/"
      pkgName + declaringClass
    case MemberName(pkg, declaringClass, _) =>
      val pkgName = if (pkg.isEmpty) "" else pkg.mkString("/") + "/"
      pkgName + declaringClass
    case ConstrName(pkg, declaringClass, innerClass, _) =>
      val pkgName = if (pkg.isEmpty) "" else pkg.mkString("/") + "/"
      pkgName + declaringClass + "$" + innerClass
  }

  def getInternalName(name: Name):  Either[List[CodegenError], String] = name match {
    case MemberName(pkg, declaringClass, memberName) =>
      val pkgName = if (pkg.isEmpty) "" else pkg.mkString("/") + "/"
      Right(pkgName + declaringClass + "$" + memberName)
    case ConstrName(pkg, declaringClass, innerClass, memberName) =>
      val pkgName = if (pkg.isEmpty) "" else pkg.mkString("/") + "/"
      Right(pkgName + declaringClass + "$" + innerClass + "$" + memberName)
    case _ =>
      CodegenError.singleton(s"Error while determining Java class name from ${name}")
  }

  def verify(classFile: ClassFile): Either[List[CodegenError], ClassFile] = {
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

  def generate(mod: Module[Meta.Typed], importedEnv: Environment): Either[List[CodegenError], List[ClassFile]] = {
    val moduleClassName = getClassName(mod.meta.name, mod.name)

    liftedDefns.set(0)

    val env = importedEnv ++ mod.environment

    val classFiles: Either[List[CodegenError], List[ClassFile]] = withClassWriter(moduleClassName) { moduleClassWriter =>
      // Persist the AST to protobuf
      moduleClassWriter.visitAttribute(InterfaceAttribute(mod.toProto.toByteArray))
      // Make the static initializer available to set field values
      withGeneratorAdapter(moduleClassWriter, "<clinit>", AsmType.getMethodDescriptor(AsmType.VOID_TYPE)) { staticInitializer =>
        mod.declarations.flatTraverse { decl =>
          newTopLevelDeclaration(moduleClassName, moduleClassWriter, staticInitializer, decl, env)
        }
      }
    }

    if (verifyCodegen)
      classFiles.flatMap(_.traverse(verify))
    else
      classFiles
  }
}
