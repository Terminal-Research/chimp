package chimp.protocol

import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}

opaque type RequestId = String | Int | Null
object RequestId:
  def apply(value: String | Int): RequestId = value
  val NullValue: RequestId = null
  def unapply(id: RequestId): Option[String | Int | Null] = Some(id)
  given encoder: Encoder[RequestId] = Encoder.instance:
    case null      => Json.Null
    case s: String => Json.fromString(s)
    case i: Int    => Json.fromInt(i)
  given decoder: Decoder[RequestId] = Decoder.instance: c =>
    c.as[String]
      .map(RequestId(_))
      .orElse(c.as[Int].map(RequestId(_)))
      .orElse:
        Either.cond(
          c.value.isNull,
          NullValue,
          DecodingFailure(
            "RequestId must be a string, integer, or null",
            c.history
          )
        )
  given codec: Codec[RequestId] = Codec.from(decoder, encoder)

opaque type ProgressToken = String | Int
object ProgressToken:
  def apply(value: String | Int): ProgressToken = value
  def unapply(token: ProgressToken): Option[String | Int] = Some(token)
  given encoder: Encoder[ProgressToken] = Encoder.instance:
    case s: String => Json.fromString(s)
    case i: Int    => Json.fromInt(i)
  given decoder: Decoder[ProgressToken] = Decoder.instance: c =>
    c.as[String].map(ProgressToken(_)).orElse(c.as[Int].map(ProgressToken(_)))
  given codec: Codec[ProgressToken] = Codec.from(decoder, encoder)

type Cursor = String
