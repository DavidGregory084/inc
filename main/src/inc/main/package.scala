package inc

import cats.data.OptionT
import inc.common.Error

import scala.Either
import scala.collection.immutable.List

package object main {
  type Compile[A] = OptionT[Either[List[Error], *], A]
}
