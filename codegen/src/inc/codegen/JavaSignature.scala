package inc.codegen

import cats.instances.either._
import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.functor._
import inc.common._
import java.lang.String
import org.objectweb.asm.{ Type => AsmType }
import org.objectweb.asm.signature.{ SignatureVisitor, SignatureWriter }
import scala.{ Either, Right, Unit }
import scala.collection.immutable.List
import scala.Predef.classOf

object JavaSignature {
  def writeType(moduleClassName: String, env: Environment, visitor: SignatureVisitor, typ: Type): Either[List[CodegenError], Unit] = typ match {
    case tv @ NamedTypeVariable(_, _, _) =>
      Right(visitor.visitTypeVariable(tv.name))
    case tv @ InferredTypeVariable(_, _, _) =>
      Right(visitor.visitTypeVariable(tv.name))
    case tc @ TypeConstructor(_, _, _) =>
      Codegen.asmTypeOf(moduleClassName, env, tc).map { asmType =>
        visitor.visitClassType(asmType.getInternalName)
        visitor.visitEnd()
      }
    // Java does not understand higher kinded type variables,
    // so just erase any type applications on type variables.
    case TypeApply(tv @ NamedTypeVariable(_, _, _), _, _, _) =>
      Right(visitor.visitTypeVariable(tv.name))
    case TypeApply(tv @ InferredTypeVariable(_, _, _), _, _, _) =>
      Right(visitor.visitTypeVariable(tv.name))
    case TypeApply(tp, params, _, _) =>
      Codegen.asmTypeOf(moduleClassName, env, tp).flatMap { asmType =>
        visitor.visitClassType(asmType.getInternalName)
        params.traverse_ { paramType =>
          val argVisitor = visitor.visitTypeArgument(SignatureVisitor.INSTANCEOF)
          writeType(moduleClassName, env, argVisitor, paramType)
        }.as {
          visitor.visitEnd()
        }
      }
  }

  def forDataDeclaration(data: Data[Meta.Typed]): String = {
    val dataSigWriter = new SignatureWriter()

    val objectClass = classOf[java.lang.Object]
    val objectInternalName = AsmType.getInternalName(objectClass)

    data.typeParams.foreach { tparam =>
      dataSigWriter.visitFormalTypeParameter(tparam.name)
      val typeBoundWriter = dataSigWriter.visitClassBound()
      typeBoundWriter.visitClassType(objectInternalName)
      typeBoundWriter.visitEnd()
    }

    val superSigWriter = dataSigWriter.visitSuperclass()
    superSigWriter.visitClassType(objectInternalName)
    superSigWriter.visitEnd()

    dataSigWriter.toString()
  }

  def forConstructorOf(dataClassName: String, data: Data[Meta.Typed]): String = {
    val constrSigWriter = new SignatureWriter()

    // Constructors have the same type parameters as their enclosing data declaration
    data.typeParams.foreach { tparam =>
      constrSigWriter.visitFormalTypeParameter(tparam.name)
      val typeBoundWriter = constrSigWriter.visitClassBound()
      typeBoundWriter.visitClassType(AsmType.getInternalName(classOf[java.lang.Object]))
      typeBoundWriter.visitEnd()
    }

    val superSigWriter = constrSigWriter.visitSuperclass()

    superSigWriter.visitClassType(dataClassName)

    data.typeParams.foreach { tparam =>
      val argWriter = superSigWriter.visitTypeArgument(SignatureVisitor.INSTANCEOF)
      argWriter.visitTypeVariable(tparam.name)
    }

    superSigWriter.visitEnd()

    constrSigWriter.toString()
  }

  def forJavaConstructor(moduleClassName: String, env: Environment, constr: DataConstructor[Meta.Typed]): Either[List[CodegenError], String] = {
    val constrConstrSigWriter = new SignatureWriter()

    constr.params.traverse_ { param =>
      val paramTypeVisitor = constrConstrSigWriter.visitParameterType()
      val paramTyp = param.meta.typ.typ
      writeType(moduleClassName, env, paramTypeVisitor, paramTyp)
    }.as {
      val retTypeVisitor = constrConstrSigWriter.visitReturnType()
      retTypeVisitor.visitBaseType(AsmType.VOID_TYPE.getDescriptor.charAt(0))

      constrConstrSigWriter.toString()
    }
  }
}
