package inc

import scala.Either
import scala.collection.immutable.List

package object typechecker {
  type Infer[A] = Either[List[TypeError], A]
}
