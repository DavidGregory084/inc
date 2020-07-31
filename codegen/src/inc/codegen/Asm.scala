package inc.codegen

import cats.syntax.either._
import inc.common._
import inc.rts.{ Unit => IncUnit }
import java.lang.{ Class, Object, String }
import java.lang.invoke.{ CallSite, LambdaMetafactory, MethodType, MethodHandle, MethodHandles }
import org.objectweb.asm.{ Attribute, ByteVector, ClassReader, ClassVisitor, ClassWriter, Label, Handle, Type => AsmType }
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.commons.Method
import org.objectweb.asm.util.CheckClassAdapter
import scala.{ Array, Byte, Char, Int, Unit }
import scala.collection.immutable.List
import scala.Predef.classOf
import org.objectweb.asm.commons.GeneratorAdapter

object Asm {
  case class InterfaceAttribute(buffer: Array[Byte]) extends Attribute("IncInterface") {
    override def read(classReader: ClassReader, offset: Int, length: Int, charBuffer: Array[Char], codeAttributeOffset: Int, labels: Array[Label]) = {
      var readOffset = offset
      var bytesRead = 0
      val attrBytes = new Array[Byte](length)
      while (readOffset < (offset + length)) {
        attrBytes(bytesRead) = classReader.readByte(readOffset).toByte
        readOffset += 1
        bytesRead += 1
      }
      InterfaceAttribute(attrBytes)
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

  val InterfaceAttributePrototype = InterfaceAttribute(Array.empty)

  val BootstrapMethodDescriptor = MethodType.methodType(
    // Return type
    classOf[CallSite],
    // Stacked by the VM
    classOf[MethodHandles.Lookup], // caller
    classOf[String],               // invokedName
    classOf[MethodType],           // invokedType
    // Must be provided
    classOf[MethodType],           // samMethodType
    classOf[MethodHandle],         // implMethod
    classOf[MethodType]            // instantiatedMethodType
  ).toMethodDescriptorString()

  val BootstrapMethodHandle = new Handle(
    H_INVOKESTATIC,
    AsmType.getInternalName(classOf[LambdaMetafactory]),
    "metafactory",
    BootstrapMethodDescriptor,
    false
  )

  def methodHandle(classEnv: ClassEnvironment, meta: Meta.Typed): Handle = {
    val name = memberName(meta.name)
    val Type.Function(tpArgs) = meta.typ.typ
    val methodTypeArgs = tpArgs.map(Asm.asmType(classEnv, _))
    val methodType = AsmType.getMethodType(methodTypeArgs.last, methodTypeArgs.init: _*)
    new Handle(
      H_INVOKESTATIC,
      moduleName(classEnv, meta.name),
      name,
      methodType.getDescriptor(),
      false
    )
  }

  def internalName(mod: Module[Meta.Typed]): String = {
    if (mod.pkg.isEmpty)
      mod.name
    else
      mod.pkg.mkString("/") + "/" + mod.name
  }

  def internalName(data: Data[Meta.Typed]): String =
    internalName(data.meta.name)

  def internalName(constr: DataConstructor[Meta.Typed]): String =
    internalName(constr.meta.name)

  def asmType(mod: Module[Meta.Typed]): AsmType =
    AsmType.getObjectType(internalName(mod))

  def formatPkg(pkg: List[String]): String = {
    if (pkg.isEmpty)
      ""
    else
      pkg.mkString("/") + "/"
  }

  def moduleName(classEnv: ClassEnvironment, name: Name): String = name match {
    case ModuleName(pkg, mod) =>
      formatPkg(pkg) + mod
    case MemberName(pkg, mod, _) =>
      formatPkg(pkg) + mod
    case DataName(pkg, mod, _) =>
      formatPkg(pkg) + mod
    case ConstrName(pkg, mod, _, _) =>
      formatPkg(pkg) + mod
    case LocalName(_) =>
      internalName(classEnv.module)
    case NoName =>
      internalName(classEnv.module)
  }

  def memberName(name: Name): String = name match {
    case ModuleName(_, _) =>
      ""
    case MemberName(_, _, nm) =>
      nm
    case DataName(_, _, _) =>
      ""
    case ConstrName(_, _, _, constr) =>
      constr
    case LocalName(nm) =>
      nm
    case NoName =>
      ""
  }

  def internalName(name: Name): String = name match {
    case ModuleName(pkg, mod) =>
      formatPkg(pkg) + mod
    case MemberName(pkg, mod, _) =>
      formatPkg(pkg) + mod
    case DataName(pkg, mod, data) =>
      formatPkg(pkg) + mod + "$" + data
    case ConstrName(pkg, mod, data, constr) =>
      formatPkg(pkg) + mod + "$" + data + "$" + constr
    case LocalName(nm) =>
      nm
    case NoName =>
      ""
  }

  def functionClass(arity: Int): Class[_] = arity match {
    case 0 => classOf[inc.rts.Function0[_]]
    case 1 => classOf[inc.rts.Function1[_, _]]
    case 2 => classOf[inc.rts.Function2[_, _, _]]
    case 3 => classOf[inc.rts.Function3[_, _, _, _]]
    case 4 => classOf[inc.rts.Function4[_, _, _, _, _]]
    case 5 => classOf[inc.rts.Function5[_, _, _, _, _, _]]
    case 6 => classOf[inc.rts.Function6[_, _, _, _, _, _, _]]
    case 7 => classOf[inc.rts.Function7[_, _, _, _, _, _, _, _]]
    case 8 => classOf[inc.rts.Function8[_, _, _, _, _, _, _, _, _]]
    case 9 => classOf[inc.rts.Function9[_, _, _, _, _, _, _, _, _, _]]
    case 10 => classOf[inc.rts.Function10[_, _, _, _, _, _, _, _, _, _, _]]
    case 11 => classOf[inc.rts.Function11[_, _, _, _, _, _, _, _, _, _, _, _]]
    case 12 => classOf[inc.rts.Function12[_, _, _, _, _, _, _, _, _, _, _, _, _]]
    case 13 => classOf[inc.rts.Function13[_, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    case 14 => classOf[inc.rts.Function14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    case 15 => classOf[inc.rts.Function15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    case 16 => classOf[inc.rts.Function16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    case 17 => classOf[inc.rts.Function17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    case 18 => classOf[inc.rts.Function18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    case 19 => classOf[inc.rts.Function19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    case 20 => classOf[inc.rts.Function20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    case 21 => classOf[inc.rts.Function21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    case 22 => classOf[inc.rts.Function22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
  }

  def boxedAsmType(classEnv: ClassEnvironment, typ: Type): AsmType = {
    typ match {
      case TypeConstructor("Int", _, _) =>
        AsmType.getType(classOf[java.lang.Integer])
      case TypeConstructor("Long", _, _) =>
        AsmType.getType(classOf[java.lang.Long])
      case TypeConstructor("Float", _, _) =>
        AsmType.getType(classOf[java.lang.Float])
      case TypeConstructor("Double", _, _) =>
        AsmType.getType(classOf[java.lang.Double])
      case TypeConstructor("Boolean", _, _) =>
        AsmType.getType(classOf[java.lang.Boolean])
      case TypeConstructor("Char", _, _) =>
        AsmType.getType(classOf[java.lang.Character])
      case TypeConstructor("String", _, _) =>
        AsmType.getType(classOf[String])
      case TypeConstructor("Unit", _, _) =>
        AsmType.getType(classOf[IncUnit])
      case Type.Function(params) =>
        AsmType.getType(functionClass(params.length - 1))
      case TypeApply(typ, _, _, _) =>
        boxedAsmType(classEnv, typ)
      case TypeConstructor(name, _, _) =>
        val intName = internalName(classEnv.typeEnvironment.names(name))
        AsmType.getObjectType(intName)
      case InferredTypeVariable(_, _, _) =>
        AsmType.getType(classOf[Object])
      case NamedTypeVariable(_, _, _) =>
        AsmType.getType(classOf[Object])
    }
  }

  def asmType(classEnv: ClassEnvironment, typ: Type): AsmType = {
    typ match {
      case TypeConstructor("Int", _, _) =>
        AsmType.INT_TYPE
      case TypeConstructor("Long", _, _) =>
        AsmType.LONG_TYPE
      case TypeConstructor("Float", _, _) =>
        AsmType.FLOAT_TYPE
      case TypeConstructor("Double", _, _) =>
        AsmType.DOUBLE_TYPE
      case TypeConstructor("Boolean", _, _) =>
        AsmType.BOOLEAN_TYPE
      case TypeConstructor("Char", _, _) =>
        AsmType.CHAR_TYPE
      case TypeConstructor("String", _, _) =>
        AsmType.getType(classOf[String])
      case TypeConstructor("Unit", _, _) =>
        AsmType.getType(classOf[IncUnit])
      case Type.Function(params) =>
        AsmType.getType(functionClass(params.length - 1))
      case TypeApply(typ, _, _, _) =>
        asmType(classEnv, typ)
      case TypeConstructor(name, _, _) =>
        val tcName = internalName(classEnv.typeEnvironment.names(name))
        AsmType.getObjectType(tcName)
      case InferredTypeVariable(_, _, _) =>
        AsmType.getType(classOf[Object])
      case NamedTypeVariable(_, _, _) =>
        AsmType.getType(classOf[Object])
    }
  }

  def asmType(classEnv: ClassEnvironment, meta: Meta.Typed): AsmType = meta match {
    case Meta.Typed(modName @ ModuleName(_, _), _, _) =>
      AsmType.getObjectType(internalName(modName))
    case Meta.Typed(_, typ, _) =>
      asmType(classEnv, typ.typ)
  }

  def staticField[A](classEnv: ClassEnvironment, fieldName: String, fieldType: AsmType, initValue: A = null): Generate[Unit] = {
    classEnv.moduleWriter.visitField(
      ACC_PUBLIC + ACC_STATIC + ACC_FINAL,
      fieldName,
      fieldType.getDescriptor(),
      null,
      initValue
    ).visitEnd()

    ().asRight
  }

  def staticFieldFrom(
    classEnv: ClassEnvironment,
    fieldName: String,
    fieldType: AsmType,
    refMod: AsmType,
    refName: String,
  ): Generate[Unit] = {
    classEnv.moduleWriter.visitField(
      ACC_PUBLIC + ACC_STATIC + ACC_FINAL,
      fieldName,
      fieldType.getDescriptor(),
      null,
      null
    ).visitEnd()

    classEnv.methodWriter.getStatic(refMod, refName, fieldType)
    classEnv.methodWriter.putStatic(asmType(classEnv, classEnv.module.meta), fieldName, fieldType)

    ().asRight
  }

  def staticMethod(
    classEnv: ClassEnvironment,
    methodName: String,
    methodType: AsmType,
    methodSignature: String,
    access: Int = ACC_STATIC
  ): GeneratorAdapter = {
    new GeneratorAdapter(
      access,
      new Method(methodName, methodType.getDescriptor()),
      methodSignature,
      null,
      classEnv.moduleWriter
    )
  }

  def staticInitializer(classWriter: ClassVisitor): GeneratorAdapter = {
    val methodDescriptor = AsmType.getMethodDescriptor(AsmType.VOID_TYPE)

    val generatorAdapter = new GeneratorAdapter(
      ACC_STATIC,
      new Method("<clinit>", methodDescriptor),
      null,
      null,
      classWriter
    )

    generatorAdapter
  }

  def addDataConstructorFunction(classEnv: ClassEnvironment, data: Data[Meta.Typed], constr: DataConstructor[Meta.Typed]): Unit = {
    val dataInternalName = Asm.internalName(data)
    val dataAsmType = AsmType.getObjectType(dataInternalName)

    val constrInternalName = Asm.internalName(constr)
    val constrAsmType = AsmType.getObjectType(constrInternalName)
    val constrParamAsmTypes = constr.params.map(param => Asm.asmType(classEnv, param.meta.typ.typ))
    val constrDescriptor = AsmType.getMethodDescriptor(AsmType.VOID_TYPE, constrParamAsmTypes: _*)

    val methodType = AsmType.getMethodType(dataAsmType, constrParamAsmTypes: _*)
    val methodSignature = JavaSignature.forMethod(classEnv, constr.meta.typ)
    val methodWriter = Asm.staticMethod(classEnv, constr.name, methodType, methodSignature, ACC_PUBLIC + ACC_STATIC)

    constr.params.foreach { param =>
      methodWriter.visitParameter(param.name, ACC_FINAL)
    }

    methodWriter.visitCode()

    methodWriter.newInstance(constrAsmType)
    methodWriter.dup()

    constr.params.zipWithIndex.foreach {
      case (_, idx) => methodWriter.loadArg(idx)
    }

    methodWriter.invokeConstructor(constrAsmType, new Method("<init>", constrDescriptor))

    methodWriter.returnValue()
    methodWriter.endMethod()
  }

  def addDataField(classEnv: ClassEnvironment, parent: Data[Meta.Typed], constrWriter: ClassWriter, param: Param[Meta.Typed]) = {
    val paramType = param.meta.typ.typ
    val paramAsmType = Asm.asmType(classEnv, paramType)
    // Check whether any type parameters appear in the field's type
    val paramGeneric = parent.meta.typ.bound.exists(_.occursIn(paramType))
    // Only fields with parametric types need a signature
    val paramSignature = if (paramGeneric) JavaSignature.forType(classEnv, paramType) else null
    constrWriter.visitField(
      ACC_PUBLIC + ACC_FINAL,
      param.name,
      paramAsmType.getDescriptor(),
      paramSignature,
      null
    ).visitEnd()
  }

  def addDataConstructor(classEnv: ClassEnvironment, parent: Data[Meta.Typed], constr: DataConstructor[Meta.Typed], constrWriter: ClassWriter): Unit = {

    val Type.Function(constrFnArgs) = constr.meta.typ.typ
    val constrFnAsmTypes = constrFnArgs.init.map(asmType(classEnv, _))
    val constrDescriptor = AsmType.getMethodDescriptor(AsmType.VOID_TYPE, constrFnAsmTypes: _*)
    val constrSignature = JavaSignature.forJavaConstructor(classEnv, constr.meta.typ.typ)

    val gen = new GeneratorAdapter(
      ACC_PUBLIC,
      new Method("<init>", constrDescriptor),
      constrSignature,
      null,
      constrWriter
    )

    // The parent constructor is a nullary void method
    val voidMethodDescriptor = AsmType.getMethodDescriptor(AsmType.VOID_TYPE)
    val parentType = AsmType.getObjectType(Asm.internalName(parent))

    // Call the parent constructor
    gen.loadThis()
    gen.invokeConstructor(parentType, new Method("<init>", voidMethodDescriptor))

    constr.params.foreach { param =>
      gen.visitParameter(param.name, ACC_FINAL)
    }

    // Put each constructor argument into its corresponding field
    constr.params.zipWithIndex.foreach {
      case (param, idx) =>
        val constrType = AsmType.getObjectType(Asm.internalName(constr))
        val paramType = Asm.asmType(classEnv, param.meta.typ.typ)
        gen.loadThis()
        gen.loadArg(idx)
        gen.putField(constrType, param.name, paramType)
    }

    gen.returnValue()
    gen.endMethod()
  }

  def visitLocals(classEnv: ClassEnvironment): Unit = {
    classEnv.localVars.toList
      .sortBy {
        case (_, local) => local.varIndex
      }.foreach {
        case (name, LocalVar(idx, desc, sig, start, end)) =>
          classEnv.methodWriter.visitLocalVariable(
            name.shortName,
            desc,
            sig,
            start,
            end,
            idx
          )
      }
  }

  def addDefaultConstructor(classWriter: ClassWriter): Unit = {
    val objectType = AsmType.getType(classOf[Object])

    // The java.lang.Object constructor is a nullary void method
    val voidMethodDescriptor = AsmType.getMethodDescriptor(AsmType.VOID_TYPE)

    val gen = new GeneratorAdapter(
      0,
      new Method("<init>", voidMethodDescriptor),
      null,
      null,
      classWriter
    )

    gen.loadThis()
    gen.invokeConstructor(objectType, new Method("<init>", voidMethodDescriptor))
    gen.returnValue()
    gen.endMethod()
  }

  def constrWriter(
    parent: Data[Meta.Typed],
    constr: DataConstructor[Meta.Typed]
  ): ClassWriter = {
    val constrClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    val constrSignature = JavaSignature.forDataConstructor(parent)

    constrClassWriter.visit(
      V1_8,
      ACC_PUBLIC + ACC_FINAL + ACC_SUPER,
      internalName(constr),
      constrSignature,
      internalName(parent),
      null
    )

    constrClassWriter
  }

  def dataWriter(data: Data[Meta.Typed]): ClassWriter = {
    val dataClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    val dataSignature = JavaSignature.forDataDeclaration(data)

    dataClassWriter.visit(
      V1_8,
      ACC_PUBLIC + ACC_ABSTRACT + ACC_SUPER,
      internalName(data),
      dataSignature,
      AsmType.getInternalName(classOf[Object]),
      null
    )

    dataClassWriter
  }

  def moduleWriter(mod: Module[Meta.Typed]): (ClassVisitor, ClassWriter) = {
    // val moduleClassWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    val moduleClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    val checkClassAdapter = new CheckClassAdapter(moduleClassWriter, false)

    checkClassAdapter.visit(
      V1_8,
      ACC_PUBLIC + ACC_SUPER,
      internalName(mod),
      null,
      AsmType.getInternalName(classOf[Object]),
      null
    )

    (checkClassAdapter, moduleClassWriter)
  }

}
