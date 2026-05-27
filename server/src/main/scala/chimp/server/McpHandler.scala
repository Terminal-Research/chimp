package chimp.server

import chimp.protocol.*
import io.circe.*
import io.circe.syntax.*
import org.slf4j.LoggerFactory
import sttp.apispec.circe.*
import sttp.model.{Header, StatusCode}
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.docs.apispec.schema.TapirSchemaToJsonSchema

/** Represents different types of HTTP responses for JSON-RPC requests. */
enum McpResponse:
  /** Response with a JSON body. */
  case JsonResponse(json: Json)

  /** Response with no body for accepted notifications. */
  case EmptyAcceptResponse

  def statusCode: StatusCode = this match
    case JsonResponse(_)     => StatusCode.Ok
    case EmptyAcceptResponse => StatusCode.Accepted

  def body: Option[Json] = this match
    case JsonResponse(json)  => Some(json)
    case EmptyAcceptResponse => None

  def withNullsDroppedDeep: McpResponse = this match
    case JsonResponse(json)  => JsonResponse(json.deepDropNullValues)
    case EmptyAcceptResponse => this

/** A decoded MCP transport request that can be handled without mounting Tapir. */
final case class McpServerRequest(
    body: Json,
    headers: Seq[Header] = Seq.empty
)

/** Server metadata and serialization options for an MCP handler. */
final case class McpServerOptions(
    name: String = "Chimp MCP server",
    version: String = "1.0.0",
    showJsonSchemaMetadata: Boolean = true,
    protocolVersion: String = McpServerOptions.DefaultProtocolVersion
)

object McpServerOptions:
  val DefaultProtocolVersion: String = ProtocolVersion.Latest.name

/** Tool and resource definitions exposed by one MCP server instance. */
final case class McpServerDefinition[F[_]](
    tools: List[ServerTool[?, F]],
    resources: List[ServerResource[F]] = Nil
)

/** Transport-agnostic MCP protocol handler.
  *
  * This class owns JSON-RPC method dispatch and protocol response shaping. HTTP
  * libraries and application routers can adapt incoming requests into
  * [[McpServerRequest]] and adapt the returned [[McpResponse]] back to their
  * transport-specific response type.
  *
  * @param definition
  *   tool and resource definitions exposed by the server
  * @param options
  *   server metadata and JSON schema rendering options
  */
class McpServerHandler[F[_]](
    definition: McpServerDefinition[F],
    options: McpServerOptions
):
  private val logger = LoggerFactory.getLogger(classOf[McpServerHandler[_]])
  private val toolsByName = definition.tools.map(t => t.name -> t).toMap
  private val resourcesByUri =
    definition.resources.map(resource => resource.uri -> resource).toMap

  private val toolDefs: List[ToolDefinition] =
    definition.tools.map(toolToDefinition)
  private val resourceDefs: List[Resource] =
    definition.resources.map(_.definition)

  /** Handle one decoded MCP transport request. */
  def handle(request: McpServerRequest)(using MonadError[F]): F[McpResponse] =
    handleJsonRpc(request.body, request.headers)

  /** Handle one JSON-RPC payload with already extracted transport headers. */
  def handleJsonRpc(
      request: Json,
      headers: Seq[Header]
  )(using MonadError[F]): F[McpResponse] =
    doHandleJsonRpc(request, headers).map: response =>
      logger.debug(
        s"Request: $request, response: ${response.statusCode}, " +
          s"body: ${response.body}"
      )
      response.withNullsDroppedDeep

  /** Converts a ServerTool to its protocol definition. */
  private def toolToDefinition(tool: ServerTool[?, F]): ToolDefinition =
    val jsonSchema =
      val base =
        TapirSchemaToJsonSchema(
          tool.inputSchema,
          markOptionsAsNullable = false
        )
      if options.showJsonSchemaMetadata then base
      else base.copy($schema = None)

    ToolDefinition(
      name = tool.name,
      description = tool.description,
      inputSchema = jsonSchema.asJson,
      annotations = tool.annotations.map: annotations =>
        ToolAnnotations(
          annotations.title,
          annotations.readOnlyHint,
          annotations.destructiveHint,
          annotations.idempotentHint,
          annotations.openWorldHint
        )
    )

  private def protocolError(
      id: RequestId,
      code: Int,
      message: String
  ): JSONRPCMessage.Error =
    logger.debug(s"Protocol error (id=$id, code=$code): $message")
    JSONRPCMessage.Error(
      id = id,
      error = JSONRPCErrorObject(code = code, message = message)
    )

  private def handleInitialize(
      params: Option[Json],
      id: RequestId
  ): JSONRPCMessage.Response =
    val negotiated =
      params
        .flatMap(_.hcursor.downField("protocolVersion").as[String].toOption)
        .map(ProtocolVersion.negotiate(_).name)
        .getOrElse(options.protocolVersion)
    val capabilities =
      ServerCapabilities(
        tools = Some(ServerToolsCapability(listChanged = Some(false))),
        resources = Option.when(definition.resources.nonEmpty)(
          ServerResourcesCapability()
        )
      )
    val result =
      InitializeResult(
        protocolVersion = negotiated,
        capabilities = capabilities,
        serverInfo = Implementation(options.name, options.version)
      )
    JSONRPCMessage.Response(id = id, result = result.asJson)

  /** Handles the 'tools/list' JSON-RPC method. */
  private def handleToolsList(id: RequestId): JSONRPCMessage.Response =
    JSONRPCMessage.Response(id = id, result = ListToolsResponse(toolDefs).asJson)

  /** Handles the 'resources/list' JSON-RPC method. */
  private def handleResourcesList(id: RequestId): JSONRPCMessage.Response =
    JSONRPCMessage.Response(
      id = id,
      result = ListResourcesResult(resourceDefs).asJson
    )

  /** Handles the 'resources/read' JSON-RPC method. */
  private def handleResourcesRead(
      params: Option[Json],
      id: RequestId,
      headers: Seq[Header]
  )(using MonadError[F]): F[JSONRPCMessage] =
    val uriOpt = params.flatMap(_.hcursor.downField("uri").as[String].toOption)
    uriOpt match
      case Some(uri) =>
        resourcesByUri.get(uri) match
          case Some(resource) =>
            resource.logic(headers).map:
              case Right(content) =>
                val normalized = normalizeResourceContent(content, resource)
                JSONRPCMessage.Response(
                  id = id,
                  result = ReadResourceResult(List(normalized)).asJson
                )
              case Left(errorMsg) =>
                protocolError(
                  id,
                  JSONRPCErrorCodes.InternalError.code,
                  errorMsg
                )
          case None =>
            protocolError(
              id,
              JSONRPCErrorCodes.MethodNotFound.code,
              s"Unknown resource: $uri"
            ).unit
      case None =>
        protocolError(
          id,
          JSONRPCErrorCodes.InvalidParams.code,
          "Missing resource uri"
        ).unit

  private def normalizeResourceContent(
      content: ResourceContents,
      resource: ServerResource[F]
  ): ResourceContents =
    content match
      case ResourceContents.Text(_, text, mimeType, meta) =>
        ResourceContents.Text(
          uri = resource.uri,
          text = text,
          mimeType = mimeType.orElse(resource.mimeType),
          _meta = meta
        )
      case ResourceContents.Blob(_, blob, mimeType, meta) =>
        ResourceContents.Blob(
          uri = resource.uri,
          blob = blob,
          mimeType = mimeType.orElse(resource.mimeType),
          _meta = meta
        )

  /** Handles the 'tools/call' JSON-RPC method. */
  private def handleToolsCall(
      params: Option[Json],
      id: RequestId,
      headers: Seq[Header]
  )(using MonadError[F]): F[JSONRPCMessage] =
    val toolNameOpt =
      params.flatMap(_.hcursor.downField("name").as[String].toOption)
    val args = params.flatMap(_.hcursor.downField("arguments").focus)
      .getOrElse(Json.obj())
    toolNameOpt match
      case Some(toolName) =>
        toolsByName.get(toolName) match
          case Some(tool) =>
            def inputSnippet = args.noSpaces.take(200)
            tool.inputDecoder.decodeJson(args) match
              case Right(decodedInput) =>
                handleDecodedInput(tool, decodedInput, id, headers)
              case Left(decodingError) =>
                protocolError(
                  id,
                  JSONRPCErrorCodes.InvalidParams.code,
                  s"Invalid arguments: ${decodingError.getMessage}. " +
                    s"Input: $inputSnippet"
                ).unit
          case None =>
            protocolError(
              id,
              JSONRPCErrorCodes.MethodNotFound.code,
              s"Unknown tool: $toolName"
            ).unit
      case None =>
        protocolError(
          id,
          JSONRPCErrorCodes.InvalidParams.code,
          "Missing tool name"
        ).unit

  /** Handles a successfully decoded tool input. */
  private def handleDecodedInput[T](
      tool: ServerTool[T, F],
      decodedInput: T,
      id: RequestId,
      headers: Seq[Header]
  )(using MonadError[F]): F[JSONRPCMessage] =
    tool
      .logic(decodedInput, headers)
      .map:
        case Right(result) =>
          val callResult =
            CallToolResult(
              content = List(ToolContent.Text(text = result)),
              isError = false
            )
          JSONRPCMessage.Response(id = id, result = callResult.asJson)
        case Left(errorMsg) =>
          val callResult =
            CallToolResult(
              content = List(ToolContent.Text(text = errorMsg)),
              isError = true
            )
          JSONRPCMessage.Response(id = id, result = callResult.asJson)

  /** Handles a JSON-RPC request, dispatching to the appropriate handler. */
  private def doHandleJsonRpc(
      request: Json,
      headers: Seq[Header]
  )(using MonadError[F]): F[McpResponse] =
    request.as[JSONRPCMessage] match
      case Left(err) =>
        val errorResponse =
          protocolError(
            RequestId("null"),
            JSONRPCErrorCodes.ParseError.code,
            s"Parse error: ${err.message}"
          )
        McpResponse.JsonResponse((errorResponse: JSONRPCMessage).asJson).unit
      case Right(JSONRPCMessage.Request(_, method, params, id)) =>
        method match
          case "tools/list" =>
            val response = handleToolsList(id)
            McpResponse.JsonResponse((response: JSONRPCMessage).asJson).unit
          case "resources/list" =>
            val response = handleResourcesList(id)
            McpResponse.JsonResponse((response: JSONRPCMessage).asJson).unit
          case "tools/call" =>
            handleToolsCall(params, id, headers).map: response =>
              McpResponse.JsonResponse((response: JSONRPCMessage).asJson)
          case "resources/read" =>
            handleResourcesRead(params, id, headers).map: response =>
              McpResponse.JsonResponse((response: JSONRPCMessage).asJson)
          case "initialize" =>
            val response = handleInitialize(params, id)
            McpResponse.JsonResponse((response: JSONRPCMessage).asJson).unit
          case "ping" =>
            val response = JSONRPCMessage.Response(id = id, result = Json.obj())
            McpResponse.JsonResponse((response: JSONRPCMessage).asJson).unit
          case other =>
            val errorResponse =
              protocolError(
                id,
                JSONRPCErrorCodes.MethodNotFound.code,
                s"Unknown method: $other"
              )
            McpResponse.JsonResponse((errorResponse: JSONRPCMessage).asJson)
              .unit
      case Right(notification: JSONRPCMessage.Notification) =>
        logger.debug(s"Received notification: ${notification.method}")
        McpResponse.EmptyAcceptResponse.unit
      case Right(_) =>
        val errorResponse =
          protocolError(
            RequestId("null"),
            JSONRPCErrorCodes.InvalidRequest.code,
            "Invalid request type"
          )
        McpResponse.JsonResponse((errorResponse: JSONRPCMessage).asJson).unit

/** Backwards-compatible wrapper around [[McpServerHandler]]. */
class McpHandler[F[_]](
    tools: List[ServerTool[?, F]],
    name: String,
    version: String,
    showJsonSchemaMetadata: Boolean,
    resources: List[ServerResource[F]] = Nil
):
  private val delegate =
    McpServerHandler(
      McpServerDefinition(tools, resources),
      McpServerOptions(
        name = name,
        version = version,
        showJsonSchemaMetadata = showJsonSchemaMetadata
      )
    )

  def handle(request: McpServerRequest)(using MonadError[F]): F[McpResponse] =
    delegate.handle(request)

  def handleJsonRpc(
      request: Json,
      headers: Seq[Header]
  )(using MonadError[F]): F[McpResponse] =
    delegate.handleJsonRpc(request, headers)
