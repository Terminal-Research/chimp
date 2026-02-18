package chimp.server

import chimp.protocol.{Resource, ResourceContents}
import sttp.model.Header
import sttp.shared.Identity

/** Describes a resource before server logic is attached. */
case class PartialResource(
    uri: String,
    name: Option[String] = None,
    description: Option[String] = None,
    mimeType: Option[String] = None
):
  def name(value: String): PartialResource = copy(name = Some(value))
  def description(value: String): PartialResource = copy(description = Some(value))
  def mimeType(value: String): PartialResource = copy(mimeType = Some(value))

  /** Combine the resource description with effectful server logic. */
  def serverLogic[F[_]](
      logic: Seq[Header] => F[Either[String, ResourceContents]]
  ): ServerResource[F] =
    ServerResource(uri, name, description, mimeType, logic)

  /** Combine the resource description with synchronous logic that receives request headers. */
  def handleWithHeaders(
      logic: Seq[Header] => Either[String, ResourceContents]
  ): ServerResource[Identity] =
    ServerResource(uri, name, description, mimeType, headers => logic(headers))

  /** Combine the resource description with synchronous logic that does not use request headers. */
  def handle(
      logic: () => Either[String, ResourceContents]
  ): ServerResource[Identity] =
    handleWithHeaders(_ => logic())

/** Creates a new MCP resource description with the given URI. */
def resource(uri: String): PartialResource = PartialResource(uri)

/** A resource that can be read by the MCP server. */
case class ServerResource[F[_]](
    uri: String,
    name: Option[String],
    description: Option[String],
    mimeType: Option[String],
    logic: Seq[Header] => F[Either[String, ResourceContents]]
):
  def definition: Resource =
    Resource(
      uri = uri,
      name = name.getOrElse(uri),
      description = description,
      mimeType = mimeType
    )
