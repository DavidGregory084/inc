package inc.codegen

import org.objectweb.asm.{ ClassWriter, Type => AsmType }
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

  def newTopLevelDeclaration(cw: ClassWriter, decl: TopLevelDeclaration[Type]) = decl match {
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
    case Let(name, Reference(_, typ), _) =>
      // TODO: Handle static initialization
      cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, descriptorFor(typ), null, null).visitEnd()
  }

  def generate(mod: Module[Type]): Either[List[CodegenError], Array[Byte]] = {
    val packageName = if (mod.pkg.isEmpty) "" else mod.pkg.mkString("", "/", "/")
    Right {
      newClass(packageName + mod.name) { cw =>
        mod.declarations.foreach { decl =>
          newTopLevelDeclaration(cw, decl)
        }
      }
    }
  }
}
