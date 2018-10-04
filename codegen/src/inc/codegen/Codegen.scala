package inc.codegen

import cats.implicits._
import java.io.{ OutputStream, PrintWriter }
import java.util.Arrays
import org.objectweb.asm.{ Attribute, ByteVector, ClassReader, ClassVisitor, ClassWriter, Label, Type => AsmType }
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.TraceClassVisitor
import org.objectweb.asm.commons.{ GeneratorAdapter, Method }

import inc.common._
import inc.rts.{ Unit => IncUnit }

case class InterfaceAttribute(buffer: Array[Byte]) extends Attribute("IncInterface") {
  override def read(classReader: ClassReader, offset: Int, length: Int, charBuffer: Array[Char], codeAttributeOffset: Int, labels: Array[Label]) = {
    InterfaceAttribute(Arrays.copyOfRange(classReader.b, offset, offset + length))
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

object Codegen {
  val InterfaceAttributePrototype = InterfaceAttribute(Array.empty)

  def print(code: Array[Byte], os: OutputStream = System.out): Unit = {
    val reader = new ClassReader(code)
    val writer = new PrintWriter(os)
    val visitor = new TraceClassVisitor(writer)
    reader.accept(visitor, ClassReader.SKIP_DEBUG)
  }

  def readInterface(code: Array[Byte]): Option[Module[NameWithType]] = {
    val reader = new ClassReader(code)
    val visitor = new InterfaceAttributeVisitor()
    reader.accept(visitor, Array(InterfaceAttributePrototype), 0)
    Option(visitor.buffer).map { buf =>
      val protobuf = proto.Module.parseFrom(buf)
      Module.fromProto(protobuf)
    }
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

  def withGeneratorAdapter(classWriter: ClassWriter, methodName: String, methodDescriptor: String)(f: GeneratorAdapter => Either[List[CodegenError], Unit]): Either[List[CodegenError], Unit] = {
    val generatorAdapter =
      new GeneratorAdapter(ACC_STATIC, new Method(methodName, methodDescriptor), null, null, classWriter)

    f(generatorAdapter).map { _ =>
      generatorAdapter.returnValue()
      generatorAdapter.endMethod()
    }
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
    case TypeConstructor(name, _) =>
      Either.catchOnly[ClassNotFoundException] {
        AsmType.getType(Class.forName(name))
      }.leftFlatMap { _ =>
        CodegenError.singleton(s"Class ${name} could not be found")
      }
    case TypeVariable(_) =>
      Right(AsmType.getType(classOf[Object]))
  }

  def newStaticField[A](classWriter: ClassWriter)(fieldName: String, fieldDescriptor: String, initialValue: A): Either[List[CodegenError], Unit] =
    Right(classWriter.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, fieldName, fieldDescriptor, null, initialValue).visitEnd())

  def newStaticFieldFrom(classWriter: ClassWriter, enclosingClass: String, staticInitializer: GeneratorAdapter)(fieldName: String, fieldDescriptor: String, referencedClass: String, referencedField: String): Either[List[CodegenError], Unit] =
    Right {
      classWriter.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, fieldName, fieldDescriptor, null, null).visitEnd()
      staticInitializer.visitFieldInsn(GETSTATIC, referencedClass, referencedField, fieldDescriptor)
      staticInitializer.visitFieldInsn(PUTSTATIC, enclosingClass, fieldName, fieldDescriptor)
    }

  def newExpr(className: String, generator: GeneratorAdapter, locals: Map[String, Int])(expr: Expr[NamePosType]): Either[List[CodegenError], Unit] = expr match {
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
        _ <- newExpr(className, generator, locals)(cond)
        _ = generator.visitJumpInsn(IFEQ, falseLabel)
        _ <- newExpr(className, generator, locals)(thenExpr)
        _ = generator.visitJumpInsn(GOTO, trueLabel)
        _ = generator.visitLabel(falseLabel)
        _ <- newExpr(className, generator, locals)(elseExpr)
        _ = generator.visitLabel(trueLabel)
      } yield ()
    case Reference(ref, nameWithType) =>
      for {
        descriptor <- descriptorFor(nameWithType.typ)
        asmType <- asmTypeOf(nameWithType.typ.typ)
        loadIns = loadInstructionFor(asmType)
        internalName = getInternalName(nameWithType.name, className)
      } yield {
        nameWithType.name match {
          case MemberName(_, _, _) =>
            generator.visitFieldInsn(GETSTATIC, internalName, ref, descriptor)
          case LocalName(nm) =>
            generator.loadArg(locals(nm))
          case NoName | ModuleName(_, _) =>
            throw new Exception("wtf")
        }
      }
    case Apply(fn, args, _) =>
      val TypeScheme(_, TypeConstructor("->", tpArgs)) = fn.meta.typ

      val getMethodName = fn.meta.name match {
        case LocalName(nm) =>
          Right(nm)
        case MemberName(_, _, nm) =>
          Right(nm)
        case _ =>
          CodegenError.singleton("Unable to retrieve name for method")
      }

      for {
        methodName <- getMethodName

        argTps <- tpArgs.init.traverse(asmTypeOf)
        retTp <- asmTypeOf(tpArgs.last)

        descriptor = AsmType.getMethodDescriptor(retTp, argTps: _*)

        internalName = getInternalName(fn.meta.name, className)

        _ <- args.zip(tpArgs).traverse_ {
          case (arg, argTp) =>
            for {
              actualArgTp <- asmTypeOf(arg.meta.typ.typ)
              _ <- newExpr(className, generator, locals)(arg)
            } yield {
              if (arg.meta.typ.typ.isPrimitive && !argTp.isPrimitive) generator.box(actualArgTp)
            }
        }
      } yield generator.visitMethodInsn(INVOKESTATIC, internalName, methodName, descriptor, false)

    case Lambda(_, _, _) =>
      ???
  }

  def newTopLevelLet(className: String, classWriter: ClassWriter, staticInitializer: GeneratorAdapter, let: Let[NamePosType]): Either[List[CodegenError], Unit] = {
    let.binding match {
      case LiteralInt(i, _) =>
        newStaticField(classWriter)(let.name, AsmType.INT_TYPE.getDescriptor, i)
      case LiteralLong(l, _) =>
        newStaticField(classWriter)(let.name, AsmType.LONG_TYPE.getDescriptor, l)
      case LiteralFloat(f, _) =>
        newStaticField(classWriter)(let.name, AsmType.FLOAT_TYPE.getDescriptor, f)
      case LiteralDouble(d, _) =>
        newStaticField(classWriter)(let.name, AsmType.DOUBLE_TYPE.getDescriptor, d)
      case LiteralBoolean(b, _) =>
        newStaticField(classWriter)(let.name, AsmType.BOOLEAN_TYPE.getDescriptor, b)
      case LiteralChar(c, _) =>
        newStaticField(classWriter)(let.name, AsmType.CHAR_TYPE.getDescriptor, c)
      case LiteralString(s, _) =>
        newStaticField(classWriter)(let.name, AsmType.getDescriptor(classOf[String]), s)
      case LiteralUnit(_) =>
        val unitClass = classOf[IncUnit]
        val descriptor = AsmType.getDescriptor(unitClass)
        val internalName = AsmType.getInternalName(unitClass)
        newStaticFieldFrom(classWriter, className, staticInitializer)(let.name, descriptor, internalName, "instance")
      case Reference(ref, nameWithType) =>
        descriptorFor(nameWithType.typ).flatMap { descriptor =>
          val internalName = getInternalName(nameWithType.name, enclosingClass = className)
          newStaticFieldFrom(classWriter, className, staticInitializer)(let.name, descriptor, internalName, ref)
        }
      case ifExpr @ If(_, _, _, nameWithType) =>
        descriptorFor(nameWithType.typ).map { descriptor =>
          classWriter.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, let.name, descriptor, null, null).visitEnd()
          newExpr(className, staticInitializer, Map.empty)(ifExpr)
          staticInitializer.visitFieldInsn(PUTSTATIC, className, let.name, descriptor)
        }
      case Lambda(params, body, nameWithType) =>
        val TypeScheme(_, TypeConstructor("->", tpArgs)) = nameWithType.typ

        val descriptorFor = tpArgs.traverse(asmTypeOf).map { args =>
          AsmType.getMethodDescriptor(args.last, args.init: _*)
        }

        descriptorFor.flatMap { descriptor =>
          withGeneratorAdapter(classWriter, let.name, descriptor) { generator =>
            params.foreach(p => generator.visitParameter(p.name, ACC_FINAL))
            newExpr(className, generator, params.map(_.name).zipWithIndex.toMap)(body)
          }
        }

      case apply @ Apply(fn, _, nameWithType) =>
        val TypeScheme(_, TypeConstructor("->", tpArgs)) = fn.meta.typ
        val to = tpArgs.last

        for {
          descriptor <- descriptorFor(nameWithType.typ)
          asmType <- asmTypeOf(let.meta.typ.typ)
          _ = classWriter.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, let.name, descriptor, null, null).visitEnd()
          _ <- newExpr(className, staticInitializer, Map.empty)(apply)
        } yield {
          if (nameWithType.typ.typ.isPrimitive && !to.isPrimitive) staticInitializer.unbox(asmType)
          if (!nameWithType.typ.typ.isPrimitive) staticInitializer.visitTypeInsn(CHECKCAST, AsmType.getType(descriptor).getInternalName)
          staticInitializer.visitFieldInsn(PUTSTATIC, className, let.name, descriptor)
        }
    }
  }

  def newTopLevelDeclaration(internalName: String, cw: ClassWriter, siv: GeneratorAdapter, decl: TopLevelDeclaration[NamePosType]): Either[List[CodegenError], Unit] =
    decl match {
      case let @ Let(_, _, _) =>
        newTopLevelLet(internalName, cw, siv, let)
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

  def generate(mod: Module[NamePosType]): Either[List[CodegenError], Array[Byte]] = {
    val className = getInternalName(mod.meta.name, mod.name)

    withClassWriter(className) { classWriter =>
      // Persist the AST to protobuf
      classWriter.visitAttribute(InterfaceAttribute(mod.toProto.toByteArray))
      // Make the static initializer available to set field values
      withGeneratorAdapter(classWriter, "<clinit>", AsmType.getMethodDescriptor(AsmType.VOID_TYPE)) { staticInitializer =>
        mod.declarations.traverse_ { decl =>
          newTopLevelDeclaration(className, classWriter, staticInitializer, decl)
        }
      }
    }
  }
}
