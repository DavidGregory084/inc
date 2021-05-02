package inc.common

import io.bullet.borer.{Codec, Decoder, Encoder}
import scala.Int

case class Pos(from: Int, to: Int) {
  def isEmpty = this == Pos.Empty
}

object Pos {
  def Empty = Pos(0, 0)
  implicit val posEncoder = Encoder.forUnit.contramap[Pos](_ => ())
  implicit val posDecoder = Decoder.forUnit.map(_ => Pos.Empty)
  implicit val posCodec = Codec(posEncoder, posDecoder)
}
