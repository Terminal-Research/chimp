package chimp.server

import chimp.protocol.*
import io.circe.*
import io.circe.syntax.*
import org.slf4j.LoggerFactory
import sttp.apispec.circe.*
import sttp.model.{Header, StatusCode}
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.Schema
import sttp.tapir.docs.apispec.schema.TapirSchemaToJsonSchema

/** Represents different types of HTTP responses for JSON-RPC requests. */
enum McpResponse:
  /** Response with a JSON body. */
  case JsonResponse(json: Json)

  /** Response with no body for accepted notifications. */
  case EmptyAcceptResponse

  /** HTTP error response for transport-level rejection. */
  case ErrorResponse(code: StatusCode, json: Option[Json] = None)

  def statusCode: StatusCode = this match
    case JsonResponse(_)     => StatusCode.Ok
    case EmptyAcceptResponse => StatusCode.Accepted
    case ErrorResponse(code, _) => code

  def body: Option[Json] = this match
    case JsonResponse(json)       => Some(json)
    case EmptyAcceptResponse      => None
    case ErrorResponse(_, json)   => json

  def withNullsDroppedDeep: McpResponse = this match
    case JsonResponse(json)      => JsonResponse(json.deepDropNullValues)
    case EmptyAcceptResponse     => this
    case ErrorResponse(code, json) =>
      ErrorResponse(code, json.map(_.deepDropNullValues))

/** Lifecycle phase associated with an MCP session. */
enum McpSessionPhase:
  case Uninitialized, Initialized, Operational

/** Metadata produced while handling an MCP request. */
final case class McpResponseMetadata(
    negotiatedProtocolVersion: Option[ProtocolVersion] = None,
    nextSessionPhase: Option[McpSessionPhase] = None
)

/** A handled MCP request with response data and protocol metadata. */
final case class McpServerResult(
    response: McpResponse,
    metadata: McpResponseMetadata = McpResponseMetadata()
)

/** A decoded MCP transport request that can be handled without mounting Tapir. */
final case class McpServerRequest(
    body: Json,
    headers: Seq[Header] = Seq.empty,
    sessionPhase: McpSessionPhase = McpSessionPhase.Operational
)

/** Server metadata and serialization options for an MCP handler. */
final case class McpServerOptions(
    name: String = "Chimp MCP server",
    version: String = "1.0.0",
    showJsonSchemaMetadata: Boolean = true,
    protocolVersion: String = McpServerOptions.DefaultProtocolVersion,
    supportedProtocolVersions: List[ProtocolVersion] =
      McpServerOptions.DefaultSupportedProtocolVersions
):
  private[server] val protocolVersionRegistry: ProtocolVersionRegistry =
    val preferredProtocolVersion = ProtocolVersion
      .from(protocolVersion)
      .getOrElse:
        throw new IllegalArgumentException(
          s"Unsupported MCP protocol version: $protocolVersion"
        )
    ProtocolVersionRegistry(supportedProtocolVersions, preferredProtocolVersion)

object McpServerOptions:
  val DefaultProtocolVersion: String = ProtocolVersion.Latest.name
  val DefaultSupportedProtocolVersions: List[ProtocolVersion] =
    ProtocolVersion.Supported

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
  private val protocolVersions = options.protocolVersionRegistry
  private val ProtocolVersionHeaderName = "MCP-Protocol-Version"
  private val toolsByName = definition.tools.map(t => t.name -> t).toMap
  private val resourcesByUri =
    definition.resources.map(resource => resource.uri -> resource).toMap

  private val toolDefs: List[ToolDefinition] =
    definition.tools.map(toolToDefinition)
  private val resourceDefs: List[Resource] =
    definition.resources.map(_.definition)

  /** Handle one decoded MCP transport request. */
  def handle(request: McpServerRequest)(using MonadError[F]): F[McpResponse] =
    handleWithMetadata(request).map(_.response)

  /** Handle one decoded MCP transport request and return protocol metadata. */
  def handleWithMetadata(
      request: McpServerRequest
  )(using MonadError[F]): F[McpServerResult] =
    handleJsonRpcWithMetadata(
      request.body,
      request.headers,
      request.sessionPhase
    )

  /** Handle one JSON-RPC payload with already extracted transport headers. */
  def handleJsonRpc(
      request: Json,
      headers: Seq[Header]
  )(using MonadError[F]): F[McpResponse] =
    handleJsonRpcWithMetadata(request, headers).map(_.response)

  /** Handle one JSON-RPC payload and return protocol metadata. */
  def handleJsonRpcWithMetadata(
      request: Json,
      headers: Seq[Header],
      sessionPhase: McpSessionPhase = McpSessionPhase.Operational
  )(using MonadError[F]): F[McpServerResult] =
    doHandleJsonRpc(request, headers, sessionPhase).map: result =>
      logger.debug(
        s"Request: $request, response: ${result.response.statusCode}, " +
          s"body: ${result.response.body}"
      )
      result.copy(response = result.response.withNullsDroppedDeep)

  /** Converts a ServerTool to its protocol definition. */
  private def toolToDefinition(tool: ServerTool[?, F]): ToolDefinition =
    ToolDefinition(
      name = tool.name,
      description = tool.description,
      inputSchema = schemaToJson(tool.inputSchema),
      outputSchema = tool.outputSchema.map(schemaToJson),
      annotations = tool.annotations.map: annotations =>
        ToolAnnotations(
          annotations.title,
          annotations.readOnlyHint,
          annotations.destructiveHint,
          annotations.idempotentHint,
          annotations.openWorldHint
        )
    )

  private def schemaToJson(schema: Schema[?]): Json =
    val base =
      TapirSchemaToJsonSchema(
        schema,
        markOptionsAsNullable = false
      )
    val schemaJson =
      if options.showJsonSchemaMetadata then base
      else base.copy($schema = None)
    schemaJson.asJson

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
  ): (JSONRPCMessage.Response, ProtocolVersion) =
    val requested =
      params
        .flatMap(_.hcursor.downField("protocolVersion").as[String].toOption)
    val negotiated = protocolVersions.negotiate(requested)
    val capabilities =
      ServerCapabilities(
        tools = Some(ServerToolsCapability(listChanged = Some(false))),
        resources = Option.when(definition.resources.nonEmpty)(
          ServerResourcesCapability()
        )
      )
    val result =
      InitializeResult(
        protocolVersion = negotiated.name,
        capabilities = capabilities,
        serverInfo = Implementation(options.name, options.version)
      )
    (JSONRPCMessage.Response(id = id, result = result.asJson), negotiated)

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
    tool.logic match
      case ServerToolLogic.Text(logic) =>
        logic(decodedInput, headers).map:
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
      case ServerToolLogic.Output(logic) =>
        logic(decodedInput, headers).map: output =>
          handleToolOutput(tool, output, id)

  private def handleToolOutput[T](
      tool: ServerTool[T, F],
      output: ToolOutput,
      id: RequestId
  ): JSONRPCMessage =
    validateToolOutput(tool, output) match
      case Right(callResult) =>
        JSONRPCMessage.Response(id = id, result = callResult.asJson)
      case Left(error) =>
        protocolError(id, JSONRPCErrorCodes.InternalError.code, error)

  private def validateToolOutput[T](
      tool: ServerTool[T, F],
      output: ToolOutput
  ): Either[String, CallToolResult] =
    if tool.outputSchema.nonEmpty && !output.isError &&
        output.structuredContent.isEmpty
    then
      Left(
        s"Tool '${tool.name}' declared outputSchema but did not return " +
          "structuredContent"
      )
    else Right(output.toCallToolResult)

  /** Handles a JSON-RPC request, dispatching to the appropriate handler. */
  private def doHandleJsonRpc(
      request: Json,
      headers: Seq[Header],
      sessionPhase: McpSessionPhase
  )(using MonadError[F]): F[McpServerResult] =
    validateProtocolVersionHeader(headers) match
      case Left(error) =>
        McpServerResult(rejectProtocolVersionHeader(error)).unit
      case Right(_) =>
        doDispatchJsonRpc(request, headers, sessionPhase)

  private def doDispatchJsonRpc(
      request: Json,
      headers: Seq[Header],
      sessionPhase: McpSessionPhase
  )(using MonadError[F]): F[McpServerResult] =
    if request.isArray then McpServerResult(rejectBatchRequest).unit
    else request.as[JSONRPCMessage] match
      case Left(err) =>
        val errorResponse =
          protocolError(
            RequestId("null"),
            JSONRPCErrorCodes.ParseError.code,
            s"Parse error: ${err.message}"
          )
        McpServerResult(
          McpResponse.JsonResponse((errorResponse: JSONRPCMessage).asJson)
        ).unit
      case Right(JSONRPCMessage.Request(_, method, params, id)) =>
        validateLifecycleRequest(method, sessionPhase) match
          case Some(error) =>
            val errorResponse =
              protocolError(id, JSONRPCErrorCodes.InvalidRequest.code, error)
            McpServerResult(
              McpResponse.JsonResponse((errorResponse: JSONRPCMessage).asJson)
            ).unit
          case None =>
            dispatchRequest(method, params, id, headers)
      case Right(notification: JSONRPCMessage.Notification) =>
        handleNotification(notification, sessionPhase).unit
      case Right(_) =>
        val errorResponse =
          protocolError(
            RequestId("null"),
            JSONRPCErrorCodes.InvalidRequest.code,
            "Invalid request type"
          )
        McpServerResult(
          McpResponse.JsonResponse((errorResponse: JSONRPCMessage).asJson)
        ).unit

  private def dispatchRequest(
      method: String,
      params: Option[Json],
      id: RequestId,
      headers: Seq[Header]
  )(using MonadError[F]): F[McpServerResult] =
    method match
      case "tools/list" =>
        val response = handleToolsList(id)
        McpServerResult(
          McpResponse.JsonResponse((response: JSONRPCMessage).asJson)
        ).unit
      case "resources/list" =>
        val response = handleResourcesList(id)
        McpServerResult(
          McpResponse.JsonResponse((response: JSONRPCMessage).asJson)
        ).unit
      case "tools/call" =>
        handleToolsCall(params, id, headers).map: response =>
          McpServerResult(
            McpResponse.JsonResponse((response: JSONRPCMessage).asJson)
          )
      case "resources/read" =>
        handleResourcesRead(params, id, headers).map: response =>
          McpServerResult(
            McpResponse.JsonResponse((response: JSONRPCMessage).asJson)
          )
      case "initialize" =>
        val (response, negotiatedVersion) = handleInitialize(params, id)
        McpServerResult(
          McpResponse.JsonResponse((response: JSONRPCMessage).asJson),
          McpResponseMetadata(
            negotiatedProtocolVersion = Some(negotiatedVersion),
            nextSessionPhase = Some(McpSessionPhase.Initialized)
          )
        ).unit
      case "ping" =>
        val response = JSONRPCMessage.Response(id = id, result = Json.obj())
        McpServerResult(
          McpResponse.JsonResponse((response: JSONRPCMessage).asJson)
        ).unit
      case other =>
        val errorResponse =
          protocolError(
            id,
            JSONRPCErrorCodes.MethodNotFound.code,
            s"Unknown method: $other"
          )
        McpServerResult(
          McpResponse.JsonResponse((errorResponse: JSONRPCMessage).asJson)
        ).unit

  private def handleNotification(
      notification: JSONRPCMessage.Notification,
      sessionPhase: McpSessionPhase
  ): McpServerResult =
    logger.debug(s"Received notification: ${notification.method}")
    validateLifecycleNotification(notification.method, sessionPhase) match
      case Some(error) =>
        McpServerResult(rejectLifecycleNotification(error))
      case None =>
        val nextPhase = Option.when(
          notification.method == "notifications/initialized"
        )(McpSessionPhase.Operational)
        McpServerResult(
          McpResponse.EmptyAcceptResponse,
          McpResponseMetadata(nextSessionPhase = nextPhase)
        )

  private def validateLifecycleRequest(
      method: String,
      sessionPhase: McpSessionPhase
  ): Option[String] =
    sessionPhase match
      case McpSessionPhase.Uninitialized
          if method != "initialize" && method != "ping" =>
        Some(s"MCP initialize must complete before request method: $method")
      case McpSessionPhase.Initialized if method != "ping" =>
        Some(
          "MCP initialized notification must be received before request " +
            s"method: $method"
        )
      case _ =>
        None

  private def validateLifecycleNotification(
      method: String,
      sessionPhase: McpSessionPhase
  ): Option[String] =
    sessionPhase match
      case McpSessionPhase.Uninitialized =>
        Some(s"MCP initialize must complete before notification: $method")
      case McpSessionPhase.Initialized
          if method != "notifications/initialized" =>
        Some(
          "MCP initialized notification must be received before " +
            s"notification: $method"
        )
      case _ =>
        None

  private def rejectLifecycleNotification(message: String): McpResponse =
    McpResponse.ErrorResponse(
      StatusCode.BadRequest,
      Some(Json.obj("error" -> Json.fromString(message)))
    )

  private def validateProtocolVersionHeader(
      headers: Seq[Header]
  ): Either[String, Option[ProtocolVersion]] =
    val versions = headers
      .collect:
        case header if header.name.equalsIgnoreCase(ProtocolVersionHeaderName) =>
          header.value.trim
      .distinct
    versions match
      case Nil =>
        Right(None)
      case version :: Nil =>
        protocolVersions.validate(version).map(Some(_))
      case _ =>
        Left("Conflicting MCP protocol version headers")

  private def rejectProtocolVersionHeader(message: String): McpResponse =
    McpResponse.ErrorResponse(
      StatusCode.BadRequest,
      Some(Json.obj("error" -> Json.fromString(message)))
    )

  private def rejectBatchRequest: McpResponse =
    val errorResponse =
      protocolError(
        RequestId("null"),
        JSONRPCErrorCodes.InvalidRequest.code,
        "JSON-RPC batch requests are not supported"
      )
    McpResponse.JsonResponse((errorResponse: JSONRPCMessage).asJson)

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

  def handleWithMetadata(
      request: McpServerRequest
  )(using MonadError[F]): F[McpServerResult] =
    delegate.handleWithMetadata(request)

  def handleJsonRpc(
      request: Json,
      headers: Seq[Header]
  )(using MonadError[F]): F[McpResponse] =
    delegate.handleJsonRpc(request, headers)

  def handleJsonRpcWithMetadata(
      request: Json,
      headers: Seq[Header]
  )(using MonadError[F]): F[McpServerResult] =
    delegate.handleJsonRpcWithMetadata(request, headers)
