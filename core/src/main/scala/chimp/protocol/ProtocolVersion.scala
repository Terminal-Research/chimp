package chimp.protocol

import io.circe.{Decoder, Encoder, Json}

enum ProtocolVersion(val name: String):
  case V2025_03_26 extends ProtocolVersion("2025-03-26")
  case V2025_06_18 extends ProtocolVersion("2025-06-18")
  case V2025_11_25 extends ProtocolVersion("2025-11-25")

  /** Whether this revision supports `Implementation.title`. */
  def supportsImplementationTitle: Boolean = this match
    case V2025_03_26 => false
    case V2025_06_18 | V2025_11_25 => true

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

  def validate(version: String): Either[String, ProtocolVersion] =
    ProtocolVersion.from(version) match
      case Some(protocolVersion) if supportedVersionSet(protocolVersion) =>
        Right(protocolVersion)
      case Some(_) =>
        Left(s"Unsupported MCP protocol version: $version")
      case None =>
        Left(s"Invalid MCP protocol version: $version")

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
