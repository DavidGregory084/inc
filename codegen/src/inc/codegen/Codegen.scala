package inc.codegen

import java.io.PrintWriter
import org.objectweb.asm.{ ClassReader, ClassWriter, MethodVisitor, Type => AsmType }
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.TraceClassVisitor

import inc.common._

object Codegen {
  def print(code: Array[Byte]): Unit = {
    val reader = new ClassReader(code)
    val writer = new PrintWriter(System.out)
    val visitor = new TraceClassVisitor(writer)
    reader.accept(visitor, ClassReader.SKIP_DEBUG)
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

  def newTopLevelDeclaration(internalName: String, cw: ClassWriter, siv: MethodVisitor, decl: TopLevelDeclaration[Type]): Either[List[CodegenError], Unit] =
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
      case Let(name, Reference(ref, typ), _) =>
        descriptorFor(typ).map { descriptor =>
          cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, descriptor, null, null).visitEnd()
          siv.visitFieldInsn(GETSTATIC, internalName, ref, descriptor)
          siv.visitFieldInsn(PUTSTATIC, internalName, name, descriptor)
        }
    }

  def generate(mod: Module[Type]): Either[List[CodegenError], Array[Byte]] = {
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

      if (writeDecls.forall(_.isRight))
        Right(())
      else
        Left(writeDecls.toList.flatMap(_.left.get))
    }
  }
}
