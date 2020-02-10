package inc

import scala.Either
import scala.collection.immutable.List

package object codegen {
  type Generate[A] = Either[List[CodegenError], A]
}
