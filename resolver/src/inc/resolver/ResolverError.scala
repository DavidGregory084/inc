package inc.resolver

import inc.common.Error

case class ResolverError(private val message: String) extends Error(message)

object ResolverError {
  def singleton(msg: String): Either[List[ResolverError], Nothing] =
    Left(List(ResolverError(msg)))
}
