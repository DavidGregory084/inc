package inc.codegen

import inc.common._
import java.lang.{ Object, String }
import org.objectweb.asm.{ Type => AsmType }
import org.objectweb.asm.signature.{ SignatureVisitor, SignatureWriter }
import scala.Unit
import scala.Predef.classOf

object JavaSignature {
  def forDataConstructor(
    parent: Data[Meta.Typed]
  ): String = {
    val visitor = new SignatureWriter

    val objectInternalName = AsmType.getInternalName(classOf[Object])

    parent.typeParams.foreach { tparam =>
      visitor.visitFormalTypeParameter(tparam.name)
      val boundVisitor = visitor.visitClassBound()
      boundVisitor.visitClassType(objectInternalName)
      boundVisitor.visitEnd()
    }

    val superClassVisitor = visitor.visitSuperclass()
    superClassVisitor.visitClassType(Asm.internalName(parent))

    parent.typeParams.foreach { tparam =>
      val argVisitor = superClassVisitor.visitTypeArgument(SignatureVisitor.INSTANCEOF)
      argVisitor.visitTypeVariable(tparam.name)
    }

    superClassVisitor.visitEnd()

    visitor.toString()
  }

  def forDataDeclaration(
    data: Data[Meta.Typed]
  ): String = {
    val visitor = new SignatureWriter

    val objectInternalName = AsmType.getInternalName(classOf[Object])

    data.typeParams.foreach { tparam =>
      visitor.visitFormalTypeParameter(tparam.name)
      val boundVisitor = visitor.visitClassBound()
      boundVisitor.visitClassType(objectInternalName)
      boundVisitor.visitEnd()
    }

    val superClassVisitor = visitor.visitSuperclass()
    superClassVisitor.visitClassType(objectInternalName)
    superClassVisitor.visitEnd()

    visitor.toString()
  }

  def forJavaConstructor(
    classEnv: ClassEnvironment,
    typ: Type
  ): String = {
    val visitor = new SignatureWriter
    val Type.Function(params) = typ

    params.init.foreach { param =>
      val paramTypeVisitor = visitor.visitParameterType()
      writeType(classEnv, paramTypeVisitor, param)
    }

    val returnTypeVisitor = visitor.visitReturnType()
    returnTypeVisitor.visitBaseType(AsmType.VOID_TYPE.getDescriptor().charAt(0))

    visitor.toString()
  }

  def forMethod(
    classEnv: ClassEnvironment,
    typ: TypeScheme
  ): String = {
    val visitor = new SignatureWriter
    val Type.Function(params) = typ.typ
    val objectType = AsmType.getInternalName(classOf[Object])

    typ.bound.foreach { tv =>
      visitor.visitFormalTypeParameter(tv.name)
      val typeBoundWriter = visitor.visitClassBound()
      typeBoundWriter.visitClassType(objectType)
      typeBoundWriter.visitEnd()
    }

    params.init.foreach { param =>
      val paramTypeVisitor = visitor.visitParameterType()
      writeType(classEnv, paramTypeVisitor, param)
    }

    val returnTypeVisitor = visitor.visitReturnType()
    writeType(classEnv, returnTypeVisitor, params.last)

    visitor.toString()
  }

  def writeBoxedType(
    classEnv: ClassEnvironment,
    visitor: SignatureVisitor,
    typ: Type
  ): Unit = typ match {
    case tv @ NamedTypeVariable(_, _, _) =>
      visitor.visitTypeVariable(tv.name)

    case tv @ InferredTypeVariable(_, _, _) =>
      visitor.visitTypeVariable(tv.name)

    case tc @ TypeConstructor(_, _, _) =>
      val tcType = Asm.boxedAsmType(classEnv, tc)
      visitor.visitClassType(tcType.getInternalName())
      visitor.visitEnd()

    case TypeApply(tv @ NamedTypeVariable(_, _, _), _, _, _) =>
      visitor.visitTypeVariable(tv.name)

    case TypeApply(tv @ InferredTypeVariable(_, _, _), _, _, _) =>
      visitor.visitTypeVariable(tv.name)

    case TypeApply(TypeConstructor("->", _, _), params, _, _) =>
      val fnType = AsmType.getType(Asm.functionClass(params.length - 1))

      visitor.visitClassType(fnType.getInternalName())

      params.foreach { param =>
        val argVisitor = visitor.visitTypeArgument(SignatureVisitor.INSTANCEOF)
        writeBoxedType(classEnv, argVisitor, param)
      }

      visitor.visitEnd()

    case TypeApply(tc, params, _, _) =>
      val tcType = Asm.boxedAsmType(classEnv, tc)

      visitor.visitClassType(tcType.getInternalName())

      params.foreach { param =>
        val argVisitor = visitor.visitTypeArgument(SignatureVisitor.INSTANCEOF)
        writeBoxedType(classEnv, argVisitor, param)
      }

      visitor.visitEnd()
  }

  def forType(classEnv: ClassEnvironment, typ: Type): String = {
    val visitor = new SignatureWriter
    JavaSignature.writeType(classEnv, visitor, typ)
    visitor.toString()
  }

  def writeType(
    classEnv: ClassEnvironment,
    visitor: SignatureVisitor,
    typ: Type
  ): Unit = typ match {
    case tv @ NamedTypeVariable(_, _, _) =>
      visitor.visitTypeVariable(tv.name)

    case tv @ InferredTypeVariable(_, _, _) =>
      visitor.visitTypeVariable(tv.name)

    case TypeConstructor("Int", _, _) =>
      visitor.visitBaseType(AsmType.INT_TYPE.getDescriptor().charAt(0))

    case TypeConstructor("Long", _, _) =>
      visitor.visitBaseType(AsmType.LONG_TYPE.getDescriptor().charAt(0))

    case TypeConstructor("Float", _, _) =>
      visitor.visitBaseType(AsmType.FLOAT_TYPE.getDescriptor().charAt(0))

    case TypeConstructor("Double", _, _) =>
      visitor.visitBaseType(AsmType.DOUBLE_TYPE.getDescriptor().charAt(0))

    case TypeConstructor("Boolean", _, _) =>
      visitor.visitBaseType(AsmType.BOOLEAN_TYPE.getDescriptor().charAt(0))

    case TypeConstructor("Char", _, _) =>
      visitor.visitBaseType(AsmType.CHAR_TYPE.getDescriptor().charAt(0))

    case tc @ TypeConstructor(_, _, _) =>
      val tcType = Asm.asmType(classEnv, tc)
      visitor.visitClassType(tcType.getInternalName())
      visitor.visitEnd()

    case TypeApply(tv @ NamedTypeVariable(_, _, _), _, _, _) =>
      visitor.visitTypeVariable(tv.name)

    case TypeApply(tv @ InferredTypeVariable(_, _, _), _, _, _) =>
      visitor.visitTypeVariable(tv.name)

    case TypeApply(TypeConstructor("->", _, _), params, _, _) =>
      val fnType = AsmType.getType(Asm.functionClass(params.length - 1))

      visitor.visitClassType(fnType.getInternalName())

      params.foreach { param =>
        val argVisitor = visitor.visitTypeArgument(SignatureVisitor.INSTANCEOF)
        writeBoxedType(classEnv, argVisitor, param)
      }

      visitor.visitEnd()

    case TypeApply(tc, params, _, _) =>
      val tcType = Asm.asmType(classEnv, tc)

      visitor.visitClassType(tcType.getInternalName())

      params.foreach { param =>
        val argVisitor = visitor.visitTypeArgument(SignatureVisitor.INSTANCEOF)
        writeBoxedType(classEnv, argVisitor, param)
      }

      visitor.visitEnd()
  }
}
