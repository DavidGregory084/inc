package inc.codegen

import inc.common._
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.commons.GeneratorAdapter
import java.lang.String
import scala.{ Int, Option, None }
import scala.collection.immutable.Map

case class ClassEnvironment(
  typeEnvironment: Environment,
  enclosingMod: Module[Meta.Typed],
  enclosingModWriter: ClassWriter,
  enclosingMethodWriter: GeneratorAdapter,
  enclosingDeclName: Option[String],
  enclosingArgs: Map[Name, Int],
  liftedLambdas: Int,
)

object ClassEnvironment {
  def empty(
    typeEnvironment: Environment,
    enclosingMod: Module[Meta.Typed],
    enclosingModWriter: ClassWriter,
    enclosingMethodWriter: GeneratorAdapter,
  ): ClassEnvironment = ClassEnvironment(
    typeEnvironment,
    enclosingMod,
    enclosingModWriter,
    enclosingMethodWriter,
    None,
    Map.empty,
    0
  )
}
