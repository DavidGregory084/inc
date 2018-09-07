package inc.codegen

import cats.implicits._
import java.io.{ OutputStream, PrintWriter }
import java.util.Arrays
import org.objectweb.asm.{ Attribute, ByteVector, ClassReader, ClassVisitor, ClassWriter, Label, MethodVisitor, Type => AsmType }
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.TraceClassVisitor

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

  def withMethodVisitor(classWriter: ClassWriter, methodName: String, methodDescriptor: String)(f: MethodVisitor => Either[List[CodegenError], Unit]): Either[List[CodegenError], Unit] = {
    val methodVisitor = classWriter.visitMethod(ACC_STATIC, methodName, methodDescriptor, null, null)

    methodVisitor.visitCode()

    f(methodVisitor).map { _ =>
      methodVisitor.visitInsn(RETURN)
      methodVisitor.visitMaxs(0, 0)
      methodVisitor.visitEnd()
    }
  }

  def descriptorFor(typ: Type): Either[List[CodegenError], String] = typ match {
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
    // case TypeVariable(_, _) =>
    //   CodegenError.singleton("A type variable was found in code generation!")
  }

  def newStaticField[A](classWriter: ClassWriter)(fieldName: String, fieldDescriptor: String, initialValue: A): Either[List[CodegenError], Unit] =
    Right(classWriter.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, fieldName, fieldDescriptor, null, initialValue).visitEnd())

  def newStaticFieldFrom(classWriter: ClassWriter, enclosingClass: String, staticInitializer: MethodVisitor)(fieldName: String, fieldDescriptor: String, referencedClass: String, referencedField: String): Either[List[CodegenError], Unit] =
    Right {
      classWriter.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, fieldName, fieldDescriptor, null, null).visitEnd()
      staticInitializer.visitFieldInsn(GETSTATIC, referencedClass, referencedField, fieldDescriptor)
      staticInitializer.visitFieldInsn(PUTSTATIC, enclosingClass, fieldName, fieldDescriptor)
    }

  def newExpr(className: String, methodVisitor: MethodVisitor)(expr: Expr[NameWithType]): Either[List[CodegenError], Unit] = expr match {
    case LiteralInt(i, _) =>
      Right(methodVisitor.visitLdcInsn(i))
    case LiteralLong(l, _) =>
      Right(methodVisitor.visitLdcInsn(l))
    case LiteralFloat(f, _) =>
      Right(methodVisitor.visitLdcInsn(f))
    case LiteralDouble(d, _) =>
      Right(methodVisitor.visitLdcInsn(d))
    case LiteralBoolean(b, _) =>
      Right(methodVisitor.visitLdcInsn(b))
    case LiteralChar(c, _) =>
      Right(methodVisitor.visitLdcInsn(c))
    case LiteralString(s, _) =>
      Right(methodVisitor.visitLdcInsn(s))
    case LiteralUnit(_) =>
      val unitClass = classOf[IncUnit]
      val descriptor = AsmType.getDescriptor(unitClass)
      val internalName = AsmType.getInternalName(unitClass)
      Right(methodVisitor.visitFieldInsn(GETSTATIC, internalName, "instance", descriptor))
    case If(cond, thenExpr, elseExpr, _) =>
      val trueLabel = new Label
      val falseLabel = new Label
      for {
        _ <- newExpr(className, methodVisitor)(cond)
        _ = methodVisitor.visitJumpInsn(IFEQ, falseLabel)
        _ <- newExpr(className, methodVisitor)(thenExpr)
        _ = methodVisitor.visitJumpInsn(GOTO, trueLabel)
        _ = methodVisitor.visitLabel(falseLabel)
        _ <- newExpr(className, methodVisitor)(elseExpr)
        _ = methodVisitor.visitLabel(trueLabel)
      } yield ()
    case Reference(ref, nameWithType) =>
      descriptorFor(nameWithType.typ).map { descriptor =>
        val internalName = getInternalName(nameWithType.name, enclosingClass = className)
        methodVisitor.visitFieldInsn(GETSTATIC, internalName, ref, descriptor)
      }
  }

  def newTopLevelLet(className: String, classWriter: ClassWriter, staticInitializer: MethodVisitor, let: Let[NameWithType]): Either[List[CodegenError], Unit] = {
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
      case ifExpr @ If(_, thenExpr, _, _) =>
        descriptorFor(thenExpr.meta.typ).map { descriptor =>
          classWriter.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, let.name, descriptor, null, null).visitEnd()
          newExpr(className, staticInitializer)(ifExpr)
          staticInitializer.visitFieldInsn(PUTSTATIC, className, let.name, descriptor)
        }
    }
  }

  def newTopLevelDeclaration(internalName: String, cw: ClassWriter, siv: MethodVisitor, decl: TopLevelDeclaration[NameWithType]): Either[List[CodegenError], Unit] =
    decl match {
      case let @ Let(_, _, _) =>
        newTopLevelLet(internalName, cw, siv, let)
    }

  def getInternalName(name: Name, enclosingClass: String) = name match {
    case NoName => enclosingClass
    case LocalName(_) => enclosingClass
    case ModuleName(pkg, declaringClass) => pkg.mkString("/") + "/" + declaringClass
    case MemberName(pkg, declaringClass, _) => pkg.mkString("/") + "/" + declaringClass
  }

  def generate(mod: Module[NameWithType]): Either[List[CodegenError], Array[Byte]] = {
    val packageName = if (mod.pkg.isEmpty) "" else mod.pkg.mkString("", "/", "/")
    val className = packageName + mod.name

    withClassWriter(className) { classWriter =>
      // Persist the AST to protobuf
      classWriter.visitAttribute(InterfaceAttribute(mod.toProto.toByteArray))
      // Make the static initializer available to set field values
      withMethodVisitor(classWriter, "<clinit>", AsmType.getMethodDescriptor(AsmType.VOID_TYPE)) { staticInitializer =>
        mod.declarations.traverse_ { decl =>
          newTopLevelDeclaration(className, classWriter, staticInitializer, decl)
        }
      }
    }
  }
}
