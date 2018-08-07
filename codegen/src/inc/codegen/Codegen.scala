package inc.codegen

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

  def newClass(name: String)(writeClassMembers: ClassWriter => Either[List[CodegenError], Unit]): Either[List[CodegenError], Array[Byte]] = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)

    cw.visit(V1_8, ACC_PUBLIC + ACC_SUPER, name, null, AsmType.getInternalName(classOf[Object]), null)

    writeClassMembers(cw).map { _  =>
      cw.visitEnd()
      cw.toByteArray()
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
    case TypeConstructor(name, _) => Right(AsmType.getDescriptor(Class.forName(name)))
    case TypeVariable(_, _) => CodegenError.singleton("A type variable was found in code generation!")
  }

  def newTopLevelDeclaration(internalName: String, cw: ClassWriter, siv: MethodVisitor, decl: TopLevelDeclaration[NameWithType]): Either[List[CodegenError], Unit] =
    decl match {
      case Let(name, LiteralInt(i, _), _) =>
        Right(cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, AsmType.INT_TYPE.getDescriptor, null, i).visitEnd())
      case Let(name, LiteralLong(l, _), _) =>
        Right(cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, AsmType.LONG_TYPE.getDescriptor, null, l).visitEnd())
      case Let(name, LiteralFloat(f, _), _) =>
        Right(cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, AsmType.FLOAT_TYPE.getDescriptor, null, f).visitEnd())
      case Let(name, LiteralDouble(d, _), _) =>
        Right(cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, AsmType.DOUBLE_TYPE.getDescriptor, null, d).visitEnd())
      case Let(name, LiteralBoolean(b, _), _) =>
        Right(cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, AsmType.BOOLEAN_TYPE.getDescriptor, null, b).visitEnd())
      case Let(name, LiteralChar(c, _), _) =>
        Right(cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, AsmType.CHAR_TYPE.getDescriptor, null, c).visitEnd())
      case Let(name, LiteralString(s, _), _) =>
        Right(cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, AsmType.getDescriptor(classOf[String]), null, s).visitEnd())
      case Let(name, LiteralUnit(_), _) =>
        Right {
          cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, AsmType.getDescriptor(classOf[IncUnit]), null, null).visitEnd()
          siv.visitFieldInsn(GETSTATIC, AsmType.getInternalName(classOf[IncUnit]), "instance", AsmType.getDescriptor(classOf[IncUnit]))
          siv.visitFieldInsn(PUTSTATIC, internalName, name, AsmType.getDescriptor(classOf[IncUnit]))
        }
      case Let(name, Reference(ref, nameWithType), _) =>
        descriptorFor(nameWithType.typ).map { descriptor =>
          cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, descriptor, null, null).visitEnd()
          siv.visitFieldInsn(GETSTATIC, internalName, ref, descriptor)
          siv.visitFieldInsn(PUTSTATIC, internalName, name, descriptor)
        }
    }

  def generate(mod: Module[NameWithType]): Either[List[CodegenError], Array[Byte]] = {
    val packageName = if (mod.pkg.isEmpty) "" else mod.pkg.mkString("", "/", "/")

    val internalName = packageName + mod.name

    newClass(internalName) { cw =>
      val siv = cw.visitMethod(ACC_STATIC, "<clinit>", AsmType.getMethodDescriptor(AsmType.VOID_TYPE), null, null)

      siv.visitCode()

      val writeDecls = mod.declarations.map { decl =>
        newTopLevelDeclaration(internalName, cw, siv, decl)
      }

      siv.visitInsn(RETURN)

      siv.visitMaxs(0, 0)

      siv.visitEnd()

      cw.visitAttribute(InterfaceAttribute(mod.toProto.toByteArray))

      if (writeDecls.forall(_.isRight))
        Right(())
      else
        Left(writeDecls.toList.flatMap(_.left.get))
    }
  }
}
