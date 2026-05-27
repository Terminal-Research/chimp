package chimp.server

import chimp.protocol.ProtocolVersion
import io.circe.Json
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.model.{Header, StatusCode}
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/** Creates Tapir endpoint descriptions for MCP Streamable HTTP requests.
  *
  * The returned endpoints include POST request handling and an explicit
  * GET endpoint that returns `405 Method Not Allowed`, for servers that do not
  * offer an SSE stream.
  *
  * @param tools
  *   The list of tools to expose.
  * @param path
  *   The path components at which to expose the MCP server.
  * @param resources
  *   The list of resources to expose via `resources/list` and `resources/read`.
  *
  * @tparam F
  *   The effect type. Might be `Identity` for a endpoints with synchronous logic.
  */
def mcpEndpoints[F[_]](
    tools: List[ServerTool[?, F]],
    path: List[String],
    name: String = "Chimp MCP server",
    version: String = "1.0.0",
    showJsonSchemaMetadata: Boolean = true,
    resources: List[ServerResource[F]] = Nil,
    protocolVersion: String = McpServerOptions.DefaultProtocolVersion,
    supportedProtocolVersions: List[ProtocolVersion] =
      McpServerOptions.DefaultSupportedProtocolVersions
): List[ServerEndpoint[Any, F]] =
  val mcpHandler =
    McpServerHandler(
      McpServerDefinition(tools, resources),
      McpServerOptions(
        name = name,
        version = version,
        showJsonSchemaMetadata = showJsonSchemaMetadata,
        protocolVersion = protocolVersion,
        supportedProtocolVersions = supportedProtocolVersions
      )
    )
  List(mcpPostEndpoint(path, mcpHandler), mcpGetEndpoint(path))

/** Creates a Tapir POST endpoint description for MCP HTTP server requests. */
def mcpEndpoint[F[_]](
    tools: List[ServerTool[?, F]],
    path: List[String],
    name: String = "Chimp MCP server",
    version: String = "1.0.0",
    showJsonSchemaMetadata: Boolean = true,
    resources: List[ServerResource[F]] = Nil,
    protocolVersion: String = McpServerOptions.DefaultProtocolVersion,
    supportedProtocolVersions: List[ProtocolVersion] =
      McpServerOptions.DefaultSupportedProtocolVersions
): ServerEndpoint[Any, F] =
  mcpEndpoints(
    tools,
    path,
    name,
    version,
    showJsonSchemaMetadata,
    resources,
    protocolVersion,
    supportedProtocolVersions
  ).head

private def mcpPostEndpoint[F[_]](
    path: List[String],
    mcpHandler: McpServerHandler[F]
): ServerEndpoint[Any, F] =
  val e = infallibleEndpoint.post
    .in(mcpPath(path))
    .in(extractFromRequest(_.headers))
    .in(jsonBody[Json])
    .out(statusCode)
    .out(jsonBody[Option[Json]])

  ServerEndpoint.public(
    e,
    me => { (input: (Seq[Header], Json)) =>
      val (headers, json) = input
      given MonadError[F] = me
      mcpHandler
        .handle(McpServerRequest(json, headers))
        .map(response => Right((response.statusCode, response.body)))
    }
  )

private def mcpGetEndpoint[F[_]](
    path: List[String]
): ServerEndpoint[Any, F] =
  val e = infallibleEndpoint.get
    .in(mcpPath(path))
    .out(statusCode)
    .out(jsonBody[Option[Json]])

  ServerEndpoint.public(
    e,
    me => { (_: Unit) =>
      given MonadError[F] = me
      Right((StatusCode.MethodNotAllowed, Option.empty[Json])).unit
    }
  )

private def mcpPath(path: List[String]): EndpointInput[Unit] =
  path.foldLeft(emptyInput)((inputSoFar, pathComponent) =>
    inputSoFar / pathComponent
  )
