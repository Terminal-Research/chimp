package chimp.protocol

import io.circe.{Decoder, Encoder, Json}

enum ProtocolVersion(val name: String):
  case V2025_06_18 extends ProtocolVersion("2025-06-18")
  case V2025_11_25 extends ProtocolVersion("2025-11-25")

final case class ProtocolVersionRegistry(
    supportedVersions: List[ProtocolVersion],
    preferredVersion: ProtocolVersion
):
  require(
    supportedVersions.nonEmpty,
    "At least one MCP protocol version must be supported"
  )
  require(
    supportedVersions.contains(preferredVersion),
    "The preferred MCP protocol version must be supported"
  )

  private val supportedVersionSet = supportedVersions.toSet

  def negotiate(requestedVersion: Option[String]): ProtocolVersion =
    requestedVersion
      .flatMap(ProtocolVersion.from)
      .filter(supportedVersionSet)
      .getOrElse(preferredVersion)

object ProtocolVersion:
  val Latest: ProtocolVersion = V2025_11_25
  val Supported: List[ProtocolVersion] = values.toList
  val DefaultRegistry: ProtocolVersionRegistry =
    ProtocolVersionRegistry(Supported, Latest)

  def from(s: String): Option[ProtocolVersion] = values.find(_.name == s)
  def negotiate(requested: String): ProtocolVersion =
    DefaultRegistry.negotiate(Some(requested))

  given Encoder[ProtocolVersion] = Encoder.instance(v => Json.fromString(v.name))
  given Decoder[ProtocolVersion] = Decoder.decodeString.emap(s => from(s).toRight(s"Unsupported protocol version: $s"))
