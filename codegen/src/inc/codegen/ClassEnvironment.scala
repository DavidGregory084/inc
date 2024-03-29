package inc.codegen

import inc.common._
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.Label
import org.objectweb.asm.commons.GeneratorAdapter

import java.lang.String
import scala.Int
import scala.None
import scala.Option
import scala.collection.immutable.Map

case class LocalVar(
  varIndex: Int,
  descriptor: String,
  signature: String,
  startLabel: Label,
  endLabel: Label
)

case class ClassEnvironment(
  typeEnvironment: Environment[Meta.Typed],
  module: Module[Meta.Typed],
  moduleWriter: ClassVisitor,
  methodWriter: GeneratorAdapter,
  declName: Option[String],
  args: Map[Name, Int],
  localVars: Map[Name, LocalVar],
  liftedLambdas: Int
)

object ClassEnvironment {
  def empty(
    typeEnvironment: Environment[Meta.Typed],
    enclosingMod: Module[Meta.Typed],
    enclosingModWriter: ClassVisitor,
    enclosingMethodWriter: GeneratorAdapter
  ): ClassEnvironment = ClassEnvironment(
    typeEnvironment,
    enclosingMod,
    enclosingModWriter,
    enclosingMethodWriter,
    None,
    Map.empty,
    Map.empty,
    0
  )
}
