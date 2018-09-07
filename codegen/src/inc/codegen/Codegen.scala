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
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)

    cw.visit(V1_8, ACC_PUBLIC + ACC_SUPER, className, null, AsmType.getInternalName(classOf[Object]), null)

    f(cw).map { _  =>
      cw.visitEnd()
      cw.toByteArray()
    }
  }

  def withMethodVisitor(cw: ClassWriter, methodName: String, methodDescriptor: String)(f: MethodVisitor => Either[List[CodegenError], Unit]): Either[List[CodegenError], Unit] = {
    val mv = cw.visitMethod(ACC_STATIC, methodName, methodDescriptor, null, null)

    mv.visitCode()

    f(mv).map { _ =>
      mv.visitInsn(RETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd()
    }
  }

  def descriptorFor(typ: Type): Either[List[CodegenError], String] = typ match {
    case TypeConstructor("Int", _) => Right(AsmType.INT_TYPE.getDescriptor)
    case TypeConstructor("Long", _) => Right(AsmType.LONG_TYPE.getDescriptor)
    case TypeConstructor("Float", _) => Right(AsmType.FLOAT_TYPE.getDescriptor)
    case TypeConstructor("Double", _) => Right(AsmType.DOUBLE_TYPE.getDescriptor)
    case TypeConstructor("Boolean", _) => Right(AsmType.BOOLEAN_TYPE.getDescriptor)
    case TypeConstructor("Char", _) => Right(AsmType.CHAR_TYPE.getDescriptor)
    case TypeConstructor("String", _) => Right(AsmType.getDescriptor(classOf[String]))
    case TypeConstructor(name, _) =>
      try {
        Right(AsmType.getDescriptor(Class.forName(name)))
      } catch {
        case _: ClassNotFoundException =>
          CodegenError.singleton(s"Class ${name} could not be found")
      }
    case TypeVariable(_, _) =>
      CodegenError.singleton("A type variable was found in code generation!")
  }

  def newTopLevelLet(internalName: String, cw: ClassWriter, siv: MethodVisitor, let: Let[NameWithType]): Either[List[CodegenError], Unit] = {
    let.binding match {
      case LiteralInt(i, _) =>
        Right(cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, let.name, AsmType.INT_TYPE.getDescriptor, null, i).visitEnd())
      case LiteralLong(l, _) =>
        Right(cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, let.name, AsmType.LONG_TYPE.getDescriptor, null, l).visitEnd())
      case LiteralFloat(f, _) =>
        Right(cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, let.name, AsmType.FLOAT_TYPE.getDescriptor, null, f).visitEnd())
      case LiteralDouble(d, _) =>
        Right(cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, let.name, AsmType.DOUBLE_TYPE.getDescriptor, null, d).visitEnd())
      case LiteralBoolean(b, _) =>
        Right(cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, let.name, AsmType.BOOLEAN_TYPE.getDescriptor, null, b).visitEnd())
      case LiteralChar(c, _) =>
        Right(cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, let.name, AsmType.CHAR_TYPE.getDescriptor, null, c).visitEnd())
      case LiteralString(s, _) =>
        Right(cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, let.name, AsmType.getDescriptor(classOf[String]), null, s).visitEnd())
      case LiteralUnit(_) =>
        Right {
          cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, let.name, AsmType.getDescriptor(classOf[IncUnit]), null, null).visitEnd()
          siv.visitFieldInsn(GETSTATIC, AsmType.getInternalName(classOf[IncUnit]), "instance", AsmType.getDescriptor(classOf[IncUnit]))
          siv.visitFieldInsn(PUTSTATIC, internalName, let.name, AsmType.getDescriptor(classOf[IncUnit]))
        }
      case Reference(ref, nameWithType) =>
        descriptorFor(nameWithType.typ).map { descriptor =>
          cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, let.name, descriptor, null, null).visitEnd()
          siv.visitFieldInsn(GETSTATIC, toInternalName(nameWithType.name, orElse = internalName), ref, descriptor)
          siv.visitFieldInsn(PUTSTATIC, internalName, let.name, descriptor)
        }
      case If(_, _, _, _) =>
        ???
    }
  }

  def newTopLevelDeclaration(internalName: String, cw: ClassWriter, siv: MethodVisitor, decl: TopLevelDeclaration[NameWithType]): Either[List[CodegenError], Unit] =
    decl match {
      case let @ Let(_, _, _) =>
        newTopLevelLet(internalName, cw, siv, let)
    }

  def toInternalName(name: Name, orElse: String) = name match {
    case NoName => orElse
    case LocalName(_) => orElse
    case ModuleName(pkg, cls) => pkg.mkString("/") + "/" + cls
    case MemberName(pkg, cls, _) => pkg.mkString("/") + "/" + cls
  }

  def generate(mod: Module[NameWithType]): Either[List[CodegenError], Array[Byte]] = {
    val packageName = if (mod.pkg.isEmpty) "" else mod.pkg.mkString("", "/", "/")

    val internalName = packageName + mod.name

    // Create a new class definition to represent this module
    withClassWriter(internalName) { cw =>
      // Persist the module AST to the class file as protobuf
      cw.visitAttribute(InterfaceAttribute(mod.toProto.toByteArray))

      // A builder for the static initializer
      //
      // We'll use this to initialize fields that are declared in the class
      //
      withMethodVisitor(cw, "<clinit>", AsmType.getMethodDescriptor(AsmType.VOID_TYPE)) { siv =>
        mod.declarations.traverse_ { decl =>
          newTopLevelDeclaration(internalName, cw, siv, decl)
        }
      }
    }
  }
}
