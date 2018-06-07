package inc.codegen

import org.objectweb.asm.{ ClassWriter, Type }
import org.objectweb.asm.Opcodes._

import inc.common._

object Codegen {
  def newClass(name: String)(writeClassMembers: ClassWriter => Unit) = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
    cw.visit(V1_8, ACC_PUBLIC + ACC_SUPER, name, null, Type.getInternalName(classOf[Object]), null)
    writeClassMembers(cw)
    cw.visitEnd()
    cw.toByteArray()
  }

  def newDeclaration(cw: ClassWriter, decl: Declaration) = decl match {
    case Let(name, LiteralInt(i)) =>
      cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, Type.INT_TYPE.getDescriptor, null, i).visitEnd()
    case Let(name, LiteralLong(l)) =>
      cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, Type.LONG_TYPE.getDescriptor, null, l).visitEnd()
    case Let(name, LiteralBoolean(b)) =>
      cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, Type.BOOLEAN_TYPE.getDescriptor, null, b).visitEnd()
    case Let(name, LiteralChar(c)) =>
      cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, Type.CHAR_TYPE.getDescriptor, null, c).visitEnd()
    case Let(name, LiteralString(s)) =>
      cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, name, Type.getDescriptor(classOf[String]), null, s).visitEnd()
  }

  def generate(mod: Module): Array[Byte] = {
    val packageName = mod.pkg.map(_ + "/").getOrElse("")
    newClass(packageName + mod.name) { cw =>
      mod.declarations.foreach { decl =>
        newDeclaration(cw, decl)
      }
    }
  }
}
