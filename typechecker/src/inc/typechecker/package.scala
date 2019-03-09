package inc

import inc.common._

import java.lang.String
import scala.Either
import scala.collection.immutable.{ List, Map }

package object typechecker {
  type Infer[A] = Either[List[TypeError], A]
  type Environment = Map[String, TypeScheme]
}
