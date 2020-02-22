package inc

import scala.Either
import scala.collection.immutable.List

package object resolver {
  type Resolve[A] = Either[List[ResolverError], A]
}
