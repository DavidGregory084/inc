package inc.common

import java.io.Serializable
import scala.Product

abstract class SyntaxTree[A] extends Product with Serializable {
  def meta: A
}
