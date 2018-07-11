package inc.codegen

import org.objectweb.asm.{ ClassWriter, MethodVisitor, Type => AsmType }
import org.objectweb.asm.Opcodes._

import inc.common._

object Codegen {
  def newClass(name: String)(writeClassMembers: ClassWriter => Unit) = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
    cw.visit(V1_8, ACC_PUBLIC + ACC_SUPER, name, null, AsmType.getInternalName(classOf[Object]), null)
    writeClassMembers(cw)
    cw.visitEnd()
    cw.toByteArray()
  }

  def descriptorFor(typ: Type) = typ match {
    case TypeConstructor("Int", _) => AsmType.INT_TYPE.getDescriptor
    case TypeConstructor("Long", _) => AsmType.LONG_TYPE.getDescriptor
    case TypeConstructor("Float", _) => AsmType.FLOAT_TYPE.getDescriptor
    case TypeConstructor("Double", _) => AsmType.DOUBLE_TYPE.getDescriptor
    case TypeConstructor("Boolean", _) => AsmType.BOOLEAN_TYPE.getDescriptor
    case TypeConstructor("Char", _) => AsmType.CHAR_TYPE.getDescriptor
    case TypeConstructor("String", _) => AsmType.getDescriptor(classOf[String])
    case TypeConstructor(_, _) => ???
    case TypeVariable(_, _) => ???
  }

  def newTopLevelDeclaration(internalName: String, cw: ClassWriter, siv: MethodVisitor, decl: TopLevelDeclaration[Type]) = decl match {
    case Let(name, LiteralInt(i, _), _) =>
      cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, AsmType.INT_TYPE.getDescriptor, null, i).visitEnd()
    case Let(name, LiteralLong(l, _), _) =>
      cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, AsmType.LONG_TYPE.getDescriptor, null, l).visitEnd()
    case Let(name, LiteralFloat(f, _), _) =>
      cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, AsmType.FLOAT_TYPE.getDescriptor, null, f).visitEnd()
    case Let(name, LiteralDouble(d, _), _) =>
      cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, AsmType.DOUBLE_TYPE.getDescriptor, null, d).visitEnd()
    case Let(name, LiteralBoolean(b, _), _) =>
      cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, AsmType.BOOLEAN_TYPE.getDescriptor, null, b).visitEnd()
    case Let(name, LiteralChar(c, _), _) =>
      cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, AsmType.CHAR_TYPE.getDescriptor, null, c).visitEnd()
    case Let(name, LiteralString(s, _), _) =>
      cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, AsmType.getDescriptor(classOf[String]), null, s).visitEnd()
    case Let(name, Reference(ref, typ), _) =>
      val descriptor = descriptorFor(typ)
      cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, descriptor, null, null).visitEnd()
      siv.visitFieldInsn(GETSTATIC, internalName, ref, descriptor)
      siv.visitFieldInsn(PUTSTATIC, internalName, name, descriptor)
  }

  def generate(mod: Module[Type]): Either[List[CodegenError], Array[Byte]] = {
    val packageName = if (mod.pkg.isEmpty) "" else mod.pkg.mkString("", "/", "/")
    Right {
      val internalName = packageName + mod.name

      newClass(internalName) { cw =>
        val siv = cw.visitMethod(ACC_STATIC, "<clinit>", AsmType.getMethodDescriptor(AsmType.VOID_TYPE), null, null)

        siv.visitCode()

        mod.declarations.foreach { decl =>
          newTopLevelDeclaration(internalName, cw, siv, decl)
        }

        siv.visitInsn(RETURN)

        siv.visitMaxs(0, 0)

        siv.visitEnd()
      }
    }
  }
}
