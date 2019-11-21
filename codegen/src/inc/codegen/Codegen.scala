package inc.codegen

import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.instances.either._
import cats.instances.list._
import inc.common._
import inc.rts.{ Unit => IncUnit }
import java.io.{ OutputStream, PrintWriter }
import java.lang.{ Class, ClassNotFoundException, Exception, IllegalStateException, Object, String, System, ThreadLocal }
import java.lang.invoke.{ CallSite, LambdaMetafactory, MethodType, MethodHandle, MethodHandles }
import org.objectweb.asm.{ Attribute, ByteVector, ClassReader, ClassVisitor, ClassWriter, Label, Handle, Type => AsmType }
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.commons.{ GeneratorAdapter, Method }
import org.objectweb.asm.signature.{ SignatureVisitor, SignatureWriter }
import org.objectweb.asm.util.{ CheckClassAdapter, TraceClassVisitor }
import scala.{ Array, Boolean, Byte, Char, Int, Unit, Option, Either, Right, StringContext }
import scala.collection.immutable.{ List, Map }
import scala.Predef.{ ArrowAssoc, augmentString, classOf }

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

  def readInterface(code: Array[Byte]): Either[List[Error], Module[NameWithType]] = {
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

  def withClassWriter(className: String)(f: ClassWriter => Either[List[CodegenError], Unit]): Either[List[CodegenError], Array[Byte]] = {
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)

    classWriter.visit(V1_8, ACC_PUBLIC + ACC_SUPER, className, null, AsmType.getInternalName(classOf[Object]), null)

    f(classWriter).map { _  =>
      classWriter.visitEnd()
      classWriter.toByteArray()
    }
  }

  def returnInstructionFor(methodDescriptor: String) = {
    AsmType.getReturnType(methodDescriptor) match {
      case AsmType.VOID_TYPE =>
        RETURN
      case AsmType.BOOLEAN_TYPE | AsmType.CHAR_TYPE | AsmType.BYTE_TYPE | AsmType.SHORT_TYPE | AsmType.INT_TYPE =>
        IRETURN
      case AsmType.LONG_TYPE =>
        LRETURN
      case AsmType.FLOAT_TYPE =>
        FRETURN
      case AsmType.DOUBLE_TYPE =>
        DRETURN
      case _ =>
        ARETURN
    }
  }

  def loadInstructionFor(asmType: AsmType) = asmType match {
    case AsmType.VOID_TYPE =>
      throw new Exception("Attempt to return void")
    case AsmType.BOOLEAN_TYPE | AsmType.CHAR_TYPE | AsmType.BYTE_TYPE | AsmType.SHORT_TYPE | AsmType.INT_TYPE =>
      ILOAD
    case AsmType.LONG_TYPE =>
      LLOAD
    case AsmType.FLOAT_TYPE =>
      FLOAD
    case AsmType.DOUBLE_TYPE =>
      DLOAD
    case _ =>
      ALOAD
  }

  def withGeneratorAdapter(
    classWriter: ClassWriter,
    methodName: String,
    methodDescriptor: String,
    signature: String = null,
    access: Int = ACC_STATIC
  )(f: GeneratorAdapter => Either[List[CodegenError], Unit]): Either[List[CodegenError], Unit] = {
    val generatorAdapter =
      new GeneratorAdapter(access, new Method(methodName, methodDescriptor), signature, null, classWriter)

    f(generatorAdapter).map { _ =>
      generatorAdapter.returnValue()
      generatorAdapter.endMethod()
    }
  }

  def functionClass(typ: TypeConstructor): Either[List[CodegenError], Class[_]] =
    (typ.typeParams.length - 1) match {
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
      case _ => CodegenError.singleton(s"Error determining function class for ${Printer.print(typ)}")
    }

  def descriptorFor(typ: TypeScheme): Either[List[CodegenError], String] = typ.typ match {
    case TypeConstructor("Int", _) =>
      Right(AsmType.INT_TYPE.getDescriptor)
    case TypeConstructor("Long", _) =>
      Right(AsmType.LONG_TYPE.getDescriptor)
    case TypeConstructor("Float", _) =>
      Right(AsmType.FLOAT_TYPE.getDescriptor)
    case TypeConstructor("Double", _) =>
      Right(AsmType.DOUBLE_TYPE.getDescriptor)
    case TypeConstructor("Boolean", _) =>
      Right(AsmType.BOOLEAN_TYPE.getDescriptor)
    case TypeConstructor("Char", _) =>
      Right(AsmType.CHAR_TYPE.getDescriptor)
    case TypeConstructor("String", _) =>
      Right(AsmType.getDescriptor(classOf[String]))
    case tyCon @ TypeConstructor("->", _) =>
      functionClass(tyCon).map(AsmType.getDescriptor)
    case TypeConstructor(name, _) =>
      Either.catchOnly[ClassNotFoundException] {
        AsmType.getDescriptor(Class.forName(name))
      }.leftFlatMap { _ =>
        CodegenError.singleton(s"Class ${name} could not be found")
      }
    case TypeVariable(_) =>
      Right(AsmType.getDescriptor(classOf[Object]))
  }

  def asmTypeOf(typ: Type): Either[List[CodegenError], AsmType] = typ match {
    case TypeConstructor("Int", _) =>
      Right(AsmType.INT_TYPE)
    case TypeConstructor("Long", _) =>
      Right(AsmType.LONG_TYPE)
    case TypeConstructor("Float", _) =>
      Right(AsmType.FLOAT_TYPE)
    case TypeConstructor("Double", _) =>
      Right(AsmType.DOUBLE_TYPE)
    case TypeConstructor("Boolean", _) =>
      Right(AsmType.BOOLEAN_TYPE)
    case TypeConstructor("Char", _) =>
      Right(AsmType.CHAR_TYPE)
    case TypeConstructor("String", _) =>
      Right(AsmType.getType(classOf[String]))
    case tyCon @ TypeConstructor("->", _) =>
      functionClass(tyCon).map(AsmType.getType)
    case TypeConstructor(name, _) =>
      Either.catchOnly[ClassNotFoundException] {
        AsmType.getType(Class.forName(name))
      }.leftFlatMap { _ =>
        CodegenError.singleton(s"Class ${name} could not be found")
      }
    case TypeVariable(_) =>
      Right(AsmType.getType(classOf[Object]))
  }

  def boxedAsmTypeOf(typ: Type): Either[List[CodegenError], AsmType] = typ match {
    case TypeConstructor("Int", _) =>
      Right(AsmType.getType(classOf[java.lang.Integer]))
    case TypeConstructor("Long", _) =>
      Right(AsmType.getType(classOf[java.lang.Long]))
    case TypeConstructor("Float", _) =>
      Right(AsmType.getType(classOf[java.lang.Float]))
    case TypeConstructor("Double", _) =>
      Right(AsmType.getType(classOf[java.lang.Double]))
    case TypeConstructor("Boolean", _) =>
      Right(AsmType.getType(classOf[java.lang.Boolean]))
    case TypeConstructor("Char", _) =>
      Right(AsmType.getType(classOf[java.lang.Character]))
    case TypeConstructor("String", _) =>
      Right(AsmType.getType(classOf[String]))
    case tyCon @ TypeConstructor("->", _) =>
      functionClass(tyCon).map(AsmType.getType)
    case TypeConstructor(name, _) =>
      Either.catchOnly[ClassNotFoundException] {
        AsmType.getType(Class.forName(name))
      }.leftFlatMap { _ =>
        CodegenError.singleton(s"Class ${name} could not be found")
      }
    case TypeVariable(_) =>
      Right(AsmType.getType(classOf[Object]))
  }

  def writeType(visitor: SignatureVisitor, typ: Type): Either[List[CodegenError], Unit] = typ match {
    case tv @ TypeVariable(_) =>
      Right(visitor.visitTypeVariable(tv.name))
    case tc @ TypeConstructor(_, params) =>
      boxedAsmTypeOf(tc).flatMap { asmType =>
        visitor.visitClassType(asmType.getInternalName)
        params.traverse_ { paramType =>
          val argVisitor = visitor.visitTypeArgument(SignatureVisitor.INSTANCEOF)
          writeType(argVisitor, paramType)
        }.as {
          visitor.visitEnd()
        }
      }
  }

  def writeSignatureFor(typeScheme: TypeScheme): Either[List[CodegenError], String] = {
    val writer = new SignatureWriter()

    typeScheme match {
      case TypeScheme(bound, TypeConstructor("->", params)) =>

        bound.foreach { b =>
          writer.visitFormalTypeParameter(b.name)
          val typeBoundWriter = writer.visitClassBound()
          typeBoundWriter.visitClassType(AsmType.getInternalName(classOf[java.lang.Object]))
          typeBoundWriter.visitEnd()
        }

        params.init.traverse_ { paramType =>
          val paramVisitor = writer.visitParameterType()
          writeType(paramVisitor, paramType)
        }.flatMap { _ =>
          val returnTypeWriter = writer.visitReturnType()
          writeType(returnTypeWriter, params.last)
        }.as {
          writer.toString()
        }

      case TypeScheme(_, tc @ TypeConstructor(_, _)) =>
        if (tc.isPrimitive)
          asmTypeOf(tc).map { asmType =>
            writer.visitBaseType(asmType.getDescriptor.head)
            writer.toString()
          }
        else
          writeType(writer, tc).as {
            writer.toString()
          }

      case TypeScheme(_, tv @ TypeVariable(_)) =>
        writeType(writer, tv).as {
          writer.toString()
        }
    }
  }

  def newStaticField[A](classWriter: ClassWriter)(fieldName: String, fieldDescriptor: String, signature: String, initialValue: A): Either[List[CodegenError], Unit] =
    Right(classWriter.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, fieldName, fieldDescriptor, signature, initialValue).visitEnd())

  def newStaticFieldFrom(classWriter: ClassWriter, enclosingClass: String, staticInitializer: GeneratorAdapter)(fieldName: String, fieldDescriptor: String, signature: String, referencedClass: String, referencedField: String): Either[List[CodegenError], Unit] =
    Right {
      classWriter.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, fieldName, fieldDescriptor, signature, null).visitEnd()
      staticInitializer.visitFieldInsn(GETSTATIC, referencedClass, referencedField, fieldDescriptor)
      staticInitializer.visitFieldInsn(PUTSTATIC, enclosingClass, fieldName, fieldDescriptor)
    }

  def newExpr(classWriter: ClassWriter, className: String, generator: GeneratorAdapter, outerName: String, arguments: Map[String, Int], locals: Map[String, Int], typeEnv: Map[String, TypeScheme])(expr: Expr[NamePosType]): Either[List[CodegenError], Unit] = expr match {
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
        _ <- newExpr(classWriter, className, generator, outerName, arguments, locals, typeEnv)(cond)
        _ = generator.visitJumpInsn(IFEQ, falseLabel)
        _ <- newExpr(classWriter, className, generator, outerName, arguments, locals, typeEnv)(thenExpr)
        _ = generator.visitJumpInsn(GOTO, trueLabel)
        _ = generator.visitLabel(falseLabel)
        _ <- newExpr(classWriter, className, generator, outerName, arguments, locals, typeEnv)(elseExpr)
        _ = generator.visitLabel(trueLabel)
      } yield ()
    case Reference(ref, nameWithType) =>
      for {
        descriptor <- descriptorFor(nameWithType.typ)
        internalName = getInternalName(nameWithType.name, className)
        _ <- nameWithType.name match {
          case MemberName(_, _, _) =>
            Right(generator.visitFieldInsn(GETSTATIC, internalName, ref, descriptor))
          case LocalName(nm) =>
            Right(generator.loadArg(arguments(nm)))
          case NoName | ModuleName(_, _) =>
            CodegenError.singleton(s"Unable to resolve reference to variable $ref")
        }
      } yield ()

    case Apply(fn, args, nameWithType) =>
      val TypeScheme(_, TypeConstructor("->", tpArgs)) = fn.meta.typ
      val objectType = AsmType.getType(classOf[Object])

      for {
        fnTp <- asmTypeOf(fn.meta.typ.typ)
        retTp <- asmTypeOf(nameWithType.typ.typ)

        descriptor = AsmType.getMethodDescriptor(objectType, tpArgs.init.as(objectType): _*)

        _ <- newExpr(classWriter, className, generator, outerName, arguments, locals, typeEnv)(fn)

        _ <- args.traverse_ { arg =>
          for {
            actualArgTp <- asmTypeOf(arg.meta.typ.typ)
            _ <- newExpr(classWriter, className, generator, outerName, arguments, locals, typeEnv)(arg)
          } yield {
            if (arg.meta.typ.typ.isPrimitive) generator.box(actualArgTp)
          }
        }

      } yield {
        generator.visitMethodInsn(INVOKEINTERFACE, fnTp.getInternalName, "apply", descriptor, true)
        if (nameWithType.typ.typ.isPrimitive) generator.unbox(retTp)
        if (!nameWithType.typ.typ.isPrimitive) generator.visitTypeInsn(CHECKCAST, retTp.getInternalName)
      }

    case lam @ Lambda(params, body, nameWithType) =>
      val TypeScheme(_, TypeConstructor("->", tpArgs)) = nameWithType.typ

      val capturedVars = lam.capturedVariables.toList
      val capturedVarsWithIdx = capturedVars.zipWithIndex
      val capturedVarTypes = capturedVars.map(_.meta.typ.typ)
      val adaptedArgTypes = capturedVarTypes ++ tpArgs

      // This descriptor points to the real function including adapted args
      val descriptorForFunction = adaptedArgTypes.traverse(asmTypeOf).map { args =>
        AsmType.getMethodDescriptor(args.last, args.init: _*)
      }

      // This is the instantiated type for the interface method (no adapted args)
      val boxedTypeForFunction = tpArgs.traverse(boxedAsmTypeOf).map { args =>
        AsmType.getMethodType(args.last, args.init: _*)
      }

      // Manufacture params so that we can prepend captured variables to the arguments list
      val prependedParams = capturedVarsWithIdx.map {
        case (v, i) =>
          val newName = "captured$" + i
          Param(newName, v.meta.copy(name = LocalName(newName)))
      }

      // Replace all references to the captured variables with the adapted args
      val replaceCaptured = capturedVarsWithIdx.map {
        case (v, i) =>
          val newName = "captured$" + i
          (v.copy(meta = v.meta.withEmptyPos), Reference(newName, v.meta.copy(name = LocalName(newName))))
      }.toMap

      for {
        functionDescriptor <- descriptorForFunction
        instantiatedFunctionType <- boxedTypeForFunction

        lambdaType <- asmTypeOf(nameWithType.typ.typ)

        capturedArgTps <- capturedVarTypes.traverse(asmTypeOf)

        liftedName = outerName + "$lifted" + liftedDefns.get

        // We need to generalize the lifted lambdas in the type environment to write the Java generic signature
        signature <- writeSignatureFor(TypeScheme.generalize(typeEnv, nameWithType.typ.typ))

        // Write out a new static method with adapted arguments
        _ <- withGeneratorAdapter(classWriter, liftedName, functionDescriptor, signature, ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC) { innerGen =>
          val allParams = prependedParams ++ params
          allParams.foreach(p => innerGen.visitParameter(p.name, ACC_FINAL))
          newExpr(classWriter, className, innerGen, liftedName + "$inner", allParams.map(_.name).zipWithIndex.toMap, locals, typeEnv)(body.replace(replaceCaptured))
        }

        // Stack the captured variables for invokedynamic
        _ <- capturedVars.traverse_ { v =>
          newExpr(classWriter, className, generator, outerName, arguments, locals, typeEnv)(v)
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

  def newTopLevelLet(className: String, classWriter: ClassWriter, staticInitializer: GeneratorAdapter, let: Let[NamePosType], typeEnv: Map[String, TypeScheme]): Either[List[CodegenError], Unit] = {
    let.binding match {
      case LiteralInt(i, _) =>
        newStaticField(classWriter)(let.name, AsmType.INT_TYPE.getDescriptor, signature = null, i)
      case LiteralLong(l, _) =>
        newStaticField(classWriter)(let.name, AsmType.LONG_TYPE.getDescriptor, signature = null, l)
      case LiteralFloat(f, _) =>
        newStaticField(classWriter)(let.name, AsmType.FLOAT_TYPE.getDescriptor, signature = null, f)
      case LiteralDouble(d, _) =>
        newStaticField(classWriter)(let.name, AsmType.DOUBLE_TYPE.getDescriptor, signature = null, d)
      case LiteralBoolean(b, _) =>
        newStaticField(classWriter)(let.name, AsmType.BOOLEAN_TYPE.getDescriptor, signature = null, b)
      case LiteralChar(c, _) =>
        newStaticField(classWriter)(let.name, AsmType.CHAR_TYPE.getDescriptor, signature = null, c)
      case LiteralString(s, _) =>
        newStaticField(classWriter)(let.name, AsmType.getDescriptor(classOf[String]), signature = null, s)
      case LiteralUnit(_) =>
        val unitClass = classOf[IncUnit]
        val descriptor = AsmType.getDescriptor(unitClass)
        val internalName = AsmType.getInternalName(unitClass)
        newStaticFieldFrom(classWriter, className, staticInitializer)(let.name, descriptor, signature = null, internalName, "instance")
      case Reference(ref, nameWithType) =>
        descriptorFor(nameWithType.typ).flatMap { descriptor =>
          val internalName = getInternalName(nameWithType.name, enclosingClass = className)
          newStaticFieldFrom(classWriter, className, staticInitializer)(let.name, descriptor, signature = null, internalName, ref)
        }
      case ifExpr @ If(_, _, _, nameWithType) =>
        for {
          descriptor <- descriptorFor(nameWithType.typ)
          _ <- newStaticField(classWriter)(let.name, descriptor, signature = null, null)
          _ <- newExpr(classWriter, className, staticInitializer, let.name, Map.empty, Map.empty, typeEnv)(ifExpr)
        } yield staticInitializer.visitFieldInsn(PUTSTATIC, className, let.name, descriptor)
      case lam @ Lambda(_, _, nameWithType) =>
        for {
          descriptor <- descriptorFor(nameWithType.typ)
          _ <- newStaticField(classWriter)(let.name, descriptor, signature = null, null)
          _ <- newExpr(classWriter, className, staticInitializer, let.name, Map.empty, Map.empty, typeEnv)(lam)
        } yield staticInitializer.visitFieldInsn(PUTSTATIC, className, let.name, descriptor)
      case apply @ Apply(_, _, nameWithType) =>
        for {
          descriptor <- descriptorFor(nameWithType.typ)
          _ <- newStaticField(classWriter)(let.name, descriptor, signature = null, null)
          _ <- newExpr(classWriter, className, staticInitializer, let.name, Map.empty, Map.empty, typeEnv)(apply)
        } yield staticInitializer.visitFieldInsn(PUTSTATIC, className, let.name, descriptor)
    }
  }

  def newTopLevelDeclaration(internalName: String, cw: ClassWriter, siv: GeneratorAdapter, decl: TopLevelDeclaration[NamePosType], typeEnv: Map[String, TypeScheme]): Either[List[CodegenError], Unit] =
    decl match {
      case let @ Let(_, _, _) =>
        newTopLevelLet(internalName, cw, siv, let, typeEnv)
    }

  def getInternalName(name: Name, enclosingClass: String) = name match {
    case NoName => enclosingClass
    case LocalName(_) => enclosingClass
    case ModuleName(pkg, declaringClass) =>
      val pkgName = if (pkg.isEmpty) "" else pkg.mkString("/") + "/"
      pkgName + declaringClass
    case MemberName(pkg, declaringClass, _) =>
      val pkgName = if (pkg.isEmpty) "" else pkg.mkString("/") + "/"
      pkgName + declaringClass
  }

  def verify(bytes: Array[Byte]): Either[List[CodegenError], Array[Byte]] = {
    // We can't do this while writing the class the first time because the max stack size and
    // local variables are not calculated in time for CheckClassAdapter by the ClassWriter
    Either.catchOnly[IllegalStateException] {
      val classReader = new ClassReader(bytes)
      val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
      val checkClass = new CheckClassAdapter(classWriter, true)
      classReader.accept(checkClass, 0)
    }.as(bytes).leftFlatMap { err =>
      CodegenError.singleton(err.getMessage)
    }
  }

  def generate(mod: Module[NamePosType]): Either[List[CodegenError], Array[Byte]] = {
    val className = getInternalName(mod.meta.name, mod.name)

    liftedDefns.set(0)

    val typeEnv = mod.declarations.map(decl => decl.name -> decl.meta.typ).toMap

    val classBytes = withClassWriter(className) { classWriter =>
      // Persist the AST to protobuf
      classWriter.visitAttribute(InterfaceAttribute(mod.toProto.toByteArray))
      // Make the static initializer available to set field values
      withGeneratorAdapter(classWriter, "<clinit>", AsmType.getMethodDescriptor(AsmType.VOID_TYPE)) { staticInitializer =>
        mod.declarations.traverse_ { decl =>
          newTopLevelDeclaration(className, classWriter, staticInitializer, decl, typeEnv)
        }
      }
    }

    if (verifyCodegen)
      classBytes.flatMap(verify)
    else
      classBytes
  }
}
