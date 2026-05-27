package chimp.server

import io.circe.Json
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint
import sttp.model.Header

/** Creates a Tapir endpoint description, which will handle MCP HTTP server requests, using the provided tools.
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
def mcpEndpoint[F[_]](
    tools: List[ServerTool[?, F]],
    path: List[String],
    name: String = "Chimp MCP server",
    version: String = "1.0.0",
    showJsonSchemaMetadata: Boolean = true,
    resources: List[ServerResource[F]] = Nil
): ServerEndpoint[Any, F] =
  val mcpHandler =
    McpServerHandler(
      McpServerDefinition(tools, resources),
      McpServerOptions(
        name = name,
        version = version,
        showJsonSchemaMetadata = showJsonSchemaMetadata
      )
    )
  val e = infallibleEndpoint.post
    .in(path.foldLeft(emptyInput)((inputSoFar, pathComponent) => inputSoFar / pathComponent))
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
