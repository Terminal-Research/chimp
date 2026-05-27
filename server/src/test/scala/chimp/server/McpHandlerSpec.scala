package chimp.server

import chimp.protocol.*
import chimp.protocol.JSONRPCMessage.given
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.{Header, StatusCode}
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity
import sttp.tapir.Schema

class McpHandlerSpec extends AnyFlatSpec with Matchers:
  import JSONRPCMessage.*
  import chimp.protocol.JSONRPCErrorCodes.*

  // Simple test input types
  case class EchoInput(message: String) derives Schema, Codec
  case class AddInput(a: Int, b: Int) derives Schema, Codec
  case class AddOutput(total: Int) derives Schema, Codec

  // Test tools
  val echoTool = tool("echo")
    .description("Echoes the input message.")
    .input[EchoInput]
    .handle(in => Right(in.message))

  val addTool = tool("add")
    .description("Adds two numbers.")
    .input[AddInput]
    .handle(in => Right((in.a + in.b).toString))

  val structuredAddTool = tool("structuredAdd")
    .description("Adds two numbers and returns structured output.")
    .output[AddOutput]
    .input[AddInput]
    .handleOutput: in =>
      val total = in.a + in.b
      ToolOutput.structured(
        List(ToolContent.Text(text = total.toString)),
        Json.obj("total" -> Json.fromInt(total))
      )

  val invalidStructuredTool = tool("invalidStructured")
    .description("Declares structured output but returns text only.")
    .output[AddOutput]
    .input[AddInput]
    .handleOutput(in => ToolOutput.text((in.a + in.b).toString))

  val errorTool = tool("fail")
    .description("Always fails.")
    .input[EchoInput]
    .handle(_ => Left("Intentional failure"))

  // Tool that echoes the header's value for testing
  case class HeaderEchoInput(dummy: String) derives Schema, Codec
  private val headerEchoTool = tool("headerEcho")
    .description("Echoes the header value if present.")
    .input[HeaderEchoInput]
    .handleWithHeaders { (_, headers) =>
      if headers.isEmpty then Right("no header")
      else
        Right(
          headers
            .map(header => s"header name: ${header.name}, header value: ${header.value}")
            .mkString(", ")
        )
    }

  private val recentConversationResource = resource("aion://conversations/recent")
    .name("Recent conversation history")
    .description("Returns recent conversation messages.")
    .mimeType("application/json")
    .handle(() =>
      Right(
        ResourceContents.Text(
          uri = "ignored-by-handler",
          text = """[{"role":"user","text":"hello"}]""",
        )
      )
    )

  private val agentCardResource = resource("aion://agent/card")
    .name("Agent card")
    .description("Returns the agent card for this server.")
    .mimeType("application/json")
    .handle(() =>
      Right(
        ResourceContents.Text(
          uri = "ignored-by-handler",
          text = """{"name":"Agent","description":"Assistant endpoint"}"""
        )
      )
    )

  val handler = McpHandler(List(echoTool, addTool, errorTool, headerEchoTool), "Chimp MCP server", "1.0.0", true)
  val handlerWithResources = McpHandler(
    List(echoTool),
    "Chimp MCP server",
    "1.0.0",
    true,
    List(recentConversationResource, agentCardResource)
  )
  val structuredHandler =
    McpHandler(List(structuredAddTool), "Chimp MCP server", "1.0.0", true)
  val invalidStructuredHandler =
    McpHandler(List(invalidStructuredTool), "Chimp MCP server", "1.0.0", true)
  val transportHandler = McpServerHandler(
    McpServerDefinition(List(headerEchoTool)),
    McpServerOptions(
      name = "Transport handler test",
      version = "1.0.0",
      protocolVersion = "2025-06-18"
    )
  )

  def parseJson(str: String): Json = parse(str).getOrElse(throw new RuntimeException("Invalid JSON"))

  given MonadError[Identity] = IdentityMonad

  // Helper function to extract JSON from McpResponse for testing
  private def extractJsonFromResponse(response: McpResponse): Json = response match
    case McpResponse.JsonResponse(json)  => json
    case McpResponse.EmptyAcceptResponse => fail("Expected JsonResponse but got EmptyAcceptResponse")
    case McpResponse.ErrorResponse(code, _) =>
      fail(s"Expected JsonResponse but got ErrorResponse($code)")

  private def extractErrorBodyFromResponse(response: McpResponse): Json =
    response match
      case McpResponse.ErrorResponse(_, Some(json)) => json
      case McpResponse.ErrorResponse(_, None) =>
        fail("Expected ErrorResponse body")
      case other => fail(s"Expected ErrorResponse but got $other")

  "McpServerHandler" should "handle decoded transport requests with headers" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("headerEcho"),
      "arguments" -> Json.obj("dummy" -> Json.fromString("irrelevant"))
    )
    val req: JSONRPCMessage =
      Request(method = "tools/call", params = Some(params), id = RequestId("transport1"))
    val transportRequest =
      McpServerRequest(
        body = req.asJson,
        headers = Seq(Header("header-name", "transport-header"))
      )
    // When
    val response = transportHandler.handle(transportRequest)
    val respJson = extractJsonFromResponse(response)
    val resp =
      respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj =
          result.as[CallToolResult].getOrElse(fail("Failed to decode result"))
        resultObj.isError shouldBe false
        resultObj.content.head shouldBe ToolContent.Text(
          "text",
          "header name: header-name, header value: transport-header"
        )
      case _ => fail("Expected Response")

  it should "return the configured protocol version from initialize" in:
    // Given
    val req: JSONRPCMessage =
      Request(method = "initialize", id = RequestId("transport-init"))
    val transportRequest = McpServerRequest(body = req.asJson)
    // When
    val response = transportHandler.handle(transportRequest)
    val respJson = extractJsonFromResponse(response)
    val resp =
      respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj =
          result.as[InitializeResult].getOrElse(fail("Failed to decode result"))
        resultObj.protocolVersion shouldBe "2025-06-18"
      case _ => fail("Expected Response")

  it should "negotiate a supported protocol version from initialize" in:
    // Given
    val params = Json.obj(
      "protocolVersion" -> Json.fromString("2025-06-18")
    )
    val req: JSONRPCMessage =
      Request(
        method = "initialize",
        params = Some(params),
        id = RequestId("negotiated-init")
      )
    // When
    val result = handler.handleJsonRpcWithMetadata(req.asJson, Seq.empty)
    val respJson = extractJsonFromResponse(result.response)
    val resp =
      respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    result.metadata.negotiatedProtocolVersion shouldBe Some(
      ProtocolVersion.V2025_06_18
    )
    resp match
      case Response(_, _, responseResult) =>
        val resultObj = responseResult
          .as[InitializeResult]
          .getOrElse(fail("Failed to decode result"))
        resultObj.protocolVersion shouldBe "2025-06-18"
      case _ => fail("Expected Response")

  it should "fall back to the preferred version when requested version is unsupported" in:
    // Given
    val pinnedHandler =
      McpServerHandler(
        McpServerDefinition(List(echoTool)),
        McpServerOptions(
          protocolVersion = "2025-06-18",
          supportedProtocolVersions = List(ProtocolVersion.V2025_06_18)
        )
      )
    val params = Json.obj(
      "protocolVersion" -> Json.fromString("2025-11-25")
    )
    val req: JSONRPCMessage =
      Request(
        method = "initialize",
        params = Some(params),
        id = RequestId("fallback-init")
      )
    // When
    val result = pinnedHandler.handleJsonRpcWithMetadata(req.asJson, Seq.empty)
    val respJson = extractJsonFromResponse(result.response)
    val resp =
      respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    result.metadata.negotiatedProtocolVersion shouldBe Some(
      ProtocolVersion.V2025_06_18
    )
    resp match
      case Response(_, _, responseResult) =>
        val resultObj = responseResult
          .as[InitializeResult]
          .getOrElse(fail("Failed to decode result"))
        resultObj.protocolVersion shouldBe "2025-06-18"
      case _ => fail("Expected Response")

  it should "return lifecycle metadata after initialize" in:
    // Given
    val req: JSONRPCMessage =
      Request(method = "initialize", id = RequestId("phase-init"))
    val transportRequest =
      McpServerRequest(
        body = req.asJson,
        sessionPhase = McpSessionPhase.Uninitialized
      )
    // When
    val result = transportHandler.handleWithMetadata(transportRequest)
    // Then
    result.metadata.nextSessionPhase shouldBe Some(McpSessionPhase.Initialized)

  it should "reject normal requests before initialize when phase is tracked" in:
    // Given
    val req: JSONRPCMessage =
      Request(method = "tools/list", id = RequestId("phase-too-early"))
    val transportRequest =
      McpServerRequest(
        body = req.asJson,
        sessionPhase = McpSessionPhase.Uninitialized
      )
    // When
    val result = handler.handleWithMetadata(transportRequest)
    val respJson = extractJsonFromResponse(result.response)
    val resp =
      respJson.as[JSONRPCMessage].getOrElse(fail("Expected error response"))
    // Then
    resp match
      case Error(_, _, error) =>
        error.code shouldBe InvalidRequest.code
        error.message should include("initialize must complete")
      case _ => fail("Expected Error")

  it should "advance to operational after initialized notification" in:
    // Given
    val req: JSONRPCMessage = Notification(method = "notifications/initialized")
    val transportRequest =
      McpServerRequest(
        body = req.asJson,
        sessionPhase = McpSessionPhase.Initialized
      )
    // When
    val result = handler.handleWithMetadata(transportRequest)
    // Then
    result.response shouldBe McpResponse.EmptyAcceptResponse
    result.metadata.nextSessionPhase shouldBe Some(McpSessionPhase.Operational)

  it should "reject normal requests before initialized notification" in:
    // Given
    val req: JSONRPCMessage =
      Request(
        method = "tools/list",
        id = RequestId("phase-before-initialized")
      )
    val transportRequest =
      McpServerRequest(
        body = req.asJson,
        sessionPhase = McpSessionPhase.Initialized
      )
    // When
    val result = handler.handleWithMetadata(transportRequest)
    val respJson = extractJsonFromResponse(result.response)
    val resp =
      respJson.as[JSONRPCMessage].getOrElse(fail("Expected error response"))
    // Then
    resp match
      case Error(_, _, error) =>
        error.code shouldBe InvalidRequest.code
        error.message should include("initialized notification")
      case _ => fail("Expected Error")

  it should "accept supported MCP protocol version headers" in:
    // Given
    val req: JSONRPCMessage =
      Request(method = "tools/list", id = RequestId("version-header-ok"))
    // When
    val response = handler.handleJsonRpc(
      req.asJson,
      Seq(Header("MCP-Protocol-Version", "2025-06-18"))
    )
    val respJson = extractJsonFromResponse(response)
    val resp =
      respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    response.statusCode shouldBe StatusCode.Ok
    resp match
      case Response(_, _, result) =>
        val resultObj =
          result.as[ListToolsResponse].getOrElse(fail("Failed to decode result"))
        resultObj.tools.map(_.name).toSet should contain("echo")
      case _ => fail("Expected Response")

  it should "reject invalid MCP protocol version headers with Bad Request" in:
    // Given
    val req: JSONRPCMessage =
      Request(method = "tools/list", id = RequestId("version-header-invalid"))
    // When
    val response = handler.handleJsonRpc(
      req.asJson,
      Seq(Header("MCP-Protocol-Version", "not-a-version"))
    )
    val errorBody = extractErrorBodyFromResponse(response)
    val errorMessage = errorBody.hcursor
      .downField("error")
      .as[String]
      .getOrElse(fail("Expected error message"))
    // Then
    response.statusCode shouldBe StatusCode.BadRequest
    errorMessage should include("Invalid MCP protocol version")

  it should "reject unsupported MCP protocol version headers with Bad Request" in:
    // Given
    val pinnedHandler =
      McpServerHandler(
        McpServerDefinition(List(echoTool)),
        McpServerOptions(
          protocolVersion = "2025-06-18",
          supportedProtocolVersions = List(ProtocolVersion.V2025_06_18)
        )
      )
    val req: JSONRPCMessage =
      Request(
        method = "tools/list",
        id = RequestId("version-header-unsupported")
      )
    // When
    val response = pinnedHandler.handleJsonRpc(
      req.asJson,
      Seq(Header("MCP-Protocol-Version", "2025-11-25"))
    )
    val errorBody = extractErrorBodyFromResponse(response)
    val errorMessage = errorBody.hcursor
      .downField("error")
      .as[String]
      .getOrElse(fail("Expected error message"))
    // Then
    response.statusCode shouldBe StatusCode.BadRequest
    errorMessage should include("Unsupported MCP protocol version")

  it should "reject JSON-RPC batch arrays without dispatching subrequests" in:
    // Given
    var callCount = 0
    val countingTool = tool("count")
      .description("Counts calls.")
      .input[EchoInput]
      .handle: input =>
        callCount = callCount + 1
        Right(input.message)
    val batchHandler =
      McpServerHandler(
        McpServerDefinition(List(countingTool)),
        McpServerOptions(protocolVersion = "2025-06-18")
      )
    val params = Json.obj(
      "name" -> Json.fromString("count"),
      "arguments" -> Json.obj("message" -> Json.fromString("run"))
    )
    val request1: JSONRPCMessage =
      Request(method = "tools/call", params = Some(params), id = RequestId("b1"))
    val request2: JSONRPCMessage =
      Request(method = "tools/call", params = Some(params), id = RequestId("b2"))
    val batch = Json.arr(request1.asJson, request2.asJson)
    // When
    val response = batchHandler.handleJsonRpc(batch, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp =
      respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Error(_, _, error) =>
        error.code shouldBe InvalidRequest.code
        error.message should include("batch requests are not supported")
        callCount shouldBe 0
      case _ => fail("Expected Error")

  "McpHandler" should "respond to initialize" in:
    // Given
    val req: JSONRPCMessage = Request(method = "initialize", id = RequestId("1"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[InitializeResult].getOrElse(fail("Failed to decode result"))
        resultObj.protocolVersion shouldBe "2025-11-25"
        resultObj.serverInfo.name should include("Chimp MCP server")
      case _ => fail("Expected Response")

    // nulls should be dropped
    respJson.hcursor.downField("result").downField("instructions").focus shouldBe None

  it should "list available tools" in:
    // Given
    val req: JSONRPCMessage = Request(method = "tools/list", id = RequestId("2"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[ListToolsResponse].getOrElse(fail("Failed to decode result"))
        resultObj.tools.map(_.name).toSet shouldBe Set("echo", "add", "fail", "headerEcho")
      case _ => fail("Expected Response")

  it should "include output schemas in tool definitions" in:
    // Given
    val req: JSONRPCMessage =
      Request(method = "tools/list", id = RequestId("structured-list"))
    val json = req.asJson
    // When
    val response = structuredHandler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp =
      respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj =
          result.as[ListToolsResponse].getOrElse(fail("Failed to decode result"))
        val toolDef = resultObj.tools.find(_.name == "structuredAdd").get
        val outputSchema =
          toolDef.outputSchema.getOrElse(fail("Expected output schema"))
        outputSchema.hcursor
          .downField("properties")
          .downField("total")
          .focus
          .isDefined shouldBe true
      case _ => fail("Expected Response")

  it should "return structured content from structured-output tools" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("structuredAdd"),
      "arguments" -> Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3))
    )
    val req: JSONRPCMessage =
      Request(
        method = "tools/call",
        params = Some(params),
        id = RequestId("structured-call")
      )
    val json = req.asJson
    // When
    val response = structuredHandler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp =
      respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj =
          result.as[CallToolResult].getOrElse(fail("Failed to decode result"))
        resultObj.isError shouldBe false
        resultObj.content.head shouldBe ToolContent.Text("text", "5")
        resultObj.structuredContent.flatMap(
          _.hcursor.downField("total").as[Int].toOption
        ) shouldBe Some(5)
      case _ => fail("Expected Response")

  it should "reject schema-declared tool output without structured content" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("invalidStructured"),
      "arguments" -> Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3))
    )
    val req: JSONRPCMessage =
      Request(
        method = "tools/call",
        params = Some(params),
        id = RequestId("structured-invalid")
      )
    val json = req.asJson
    // When
    val response = invalidStructuredHandler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp =
      respJson.as[JSONRPCMessage].getOrElse(fail("Expected error response"))
    // Then
    resp match
      case Error(_, _, error) =>
        error.code shouldBe InternalError.code
        error.message should include("declared outputSchema")
      case _ => fail("Expected Error")

  it should "advertise resources capability when resources are configured" in:
    // Given
    val req: JSONRPCMessage =
      Request(method = "initialize", id = RequestId("resources-init"))
    val json = req.asJson
    // When
    val response = handlerWithResources.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj =
          result.as[InitializeResult].getOrElse(fail("Failed to decode result"))
        resultObj.capabilities.resources shouldBe Some(
          ServerResourcesCapability()
        )
      case _ => fail("Expected Response")

  it should "list available resources" in:
    // Given
    val req: JSONRPCMessage =
      Request(method = "resources/list", id = RequestId("resources-list"))
    val json = req.asJson
    // When
    val response = handlerWithResources.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj =
          result.as[ListResourcesResult].getOrElse(fail("Failed to decode result"))
        resultObj.resources.map(_.uri).toSet shouldBe Set(
          "aion://conversations/recent",
          "aion://agent/card"
        )
      case _ => fail("Expected Response")

  it should "read an available resource" in:
    // Given
    val params = Json.obj(
      "uri" -> Json.fromString("aion://agent/card")
    )
    val req: JSONRPCMessage = Request(
      method = "resources/read",
      params = Some(params),
      id = RequestId("resource-read")
    )
    val json = req.asJson
    // When
    val response = handlerWithResources.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj =
          result.as[ReadResourceResult].getOrElse(fail("Failed to decode result"))
        resultObj.contents should have length 1
        resultObj.contents.head match
          case ResourceContents.Text(uri, text, mimeType, _) =>
            uri shouldBe "aion://agent/card"
            mimeType shouldBe Some("application/json")
            text should include("Agent")
          case _ => fail("Expected text resource content")
      case _ => fail("Expected Response")

  it should "return an error for unknown resource" in:
    // Given
    val params = Json.obj(
      "uri" -> Json.fromString("aion://unknown/resource")
    )
    val req: JSONRPCMessage = Request(
      method = "resources/read",
      params = Some(params),
      id = RequestId("resource-missing")
    )
    val json = req.asJson
    // When
    val response = handlerWithResources.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Expected error response"))
    // Then
    resp match
      case Error(_, _, error) =>
        error.code shouldBe MethodNotFound.code
        error.message should include("Unknown resource")
      case _ => fail("Expected Error")

  it should "call a tool successfully (echo)" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("echo"),
      "arguments" -> Json.obj("message" -> Json.fromString("hello"))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("3"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[CallToolResult].getOrElse(fail("Failed to decode result"))
        resultObj.isError shouldBe false
        resultObj.content should have length 1
        resultObj.content.head shouldBe ToolContent.Text("text", "hello")
      case _ => fail("Expected Response")

  it should "call a tool successfully (add)" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("add"),
      "arguments" -> Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("4"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[CallToolResult].getOrElse(fail("Failed to decode result"))
        resultObj.isError shouldBe false
        resultObj.content.head shouldBe ToolContent.Text("text", "5")
      case _ => fail("Expected Response")

  it should "accept notifications and return EmptyAcceptResponse" in:
    // Given
    val req: JSONRPCMessage = Notification(method = "notifications/initialized")
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    // Then
    // Notifications should return EmptyAcceptResponse to indicate no body should be sent
    response shouldBe McpResponse.EmptyAcceptResponse

  it should "accept different notification types and return EmptyAcceptResponse" in:
    // Given
    val req: JSONRPCMessage = Notification(method = "notifications/tools/list_changed")
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    // Then
    // All notifications should return EmptyAcceptResponse to indicate no body should be sent
    response shouldBe McpResponse.EmptyAcceptResponse

  it should "return an error for unknown tool" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("unknown"),
      "arguments" -> Json.obj("foo" -> Json.fromString("bar"))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("5"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Expected error response"))
    // Then
    resp match
      case Error(_, _, error) =>
        error.code shouldBe MethodNotFound.code
        error.message should include("Unknown tool")
      case _ => fail("Expected Error")

  it should "return an error for invalid arguments" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("add"),
      "arguments" -> Json.obj("a" -> Json.fromString("notAnInt"), "b" -> Json.fromInt(3))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("6"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Expected error response"))
    // Then
    resp match
      case Error(_, _, error) =>
        error.code shouldBe InvalidParams.code
        error.message should include("Invalid arguments")
      case _ => fail("Expected Error")

  it should "return an error when required fields are missing (no arguments object)" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("add")
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("7"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Expected error response"))
    // Then
    resp match
      case Error(_, _, error) =>
        error.code shouldBe InvalidParams.code
        error.message should include("Invalid arguments")
      case _ => fail("Expected Error")

  it should "return an error for missing tool name" in:
    // Given
    val params = Json.obj(
      // missing 'name'
      "arguments" -> Json.obj("message" -> Json.fromString("hello"))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("8"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Expected error response"))
    // Then
    resp match
      case Error(_, _, error) =>
        error.code shouldBe InvalidParams.code
        error.message should include("Missing tool name")
      case _ => fail("Expected Error")

  it should "return an error for tool logic failure" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("fail"),
      "arguments" -> Json.obj("message" -> Json.fromString("test"))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("9"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[CallToolResult].getOrElse(fail("Failed to decode result"))
        resultObj.isError shouldBe true
        resultObj.content.head shouldBe ToolContent.Text("text", "Intentional failure")
      case _ => fail("Expected Response")

  it should "return an error for unknown method" in:
    // Given
    val req: JSONRPCMessage = Request(method = "not/a/real/method", id = RequestId("10"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Expected error response"))
    // Then
    resp match
      case Error(_, _, error) =>
        error.code shouldBe MethodNotFound.code
        error.message should include("Unknown method")
      case _ => fail("Expected Error")

  it should "call a tool with a header and receive the header's value in the response" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("headerEcho"),
      "arguments" -> Json.obj("dummy" -> Json.fromString("irrelevant"))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("header1"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq(Header("header-name", "my-secret-header")))
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[CallToolResult].getOrElse(fail("Failed to decode result"))
        resultObj.isError shouldBe false
        resultObj.content.head shouldBe ToolContent.Text("text", "header name: header-name, header value: my-secret-header")
      case _ => fail("Expected Response")

  it should "call a tool with a header and receive multiple header's values in the response" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("headerEcho"),
      "arguments" -> Json.obj("dummy" -> Json.fromString("irrelevant"))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("header1"))
    val json = req.asJson
    // When
    val response =
      handler.handleJsonRpc(json, Seq(Header("header-name", "my-secret-header"), Header("another-header-name", "another-secret-header")))
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[CallToolResult].getOrElse(fail("Failed to decode result"))
        resultObj.isError shouldBe false
        resultObj.content.head shouldBe ToolContent.Text(
          "text",
          "header name: header-name, header value: my-secret-header, header name: another-header-name, header value: another-secret-header"
        )
      case _ => fail("Expected Response")

  it should "call a tool without a header value and receive 'no header' in the response" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("headerEcho"),
      "arguments" -> Json.obj("dummy" -> Json.fromString("irrelevant"))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("header2"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[CallToolResult].getOrElse(fail("Failed to decode result"))
        resultObj.isError shouldBe false
        resultObj.content.head shouldBe ToolContent.Text("text", "no header")
      case _ => fail("Expected Response")

  it should "not use type arrays for optional fields in JSON schema" in:
    // Given - a tool with optional fields
    case class OptionalFieldInput(requiredField: String, optionalField: Option[Long]) derives Schema, Codec
    val optionalTool = tool("optionalTest")
      .description("Test tool with optional fields.")
      .input[OptionalFieldInput]
      .handle(_ => Right("ok"))

    val handlerWithOptional = McpHandler(List(optionalTool), "Test", "1.0.0", true)

    val req: JSONRPCMessage = Request(method = "tools/list", id = RequestId("opt1"))
    val json = req.asJson
    // When
    val response = handlerWithOptional.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[ListToolsResponse].getOrElse(fail("Failed to decode result"))
        val toolDef = resultObj.tools.find(_.name == "optionalTest").get
        val inputSchema = toolDef.inputSchema

        // Check that optionalField does NOT use ["integer", "null"] type array
        // Claude API rejects this format - it should just be "integer" with the field not in required
        val optionalFieldType = inputSchema.hcursor
          .downField("properties")
          .downField("optionalField")
          .downField("type")
          .focus

        optionalFieldType match
          case Some(typeValue) =>
            // Should be a simple string "integer", not an array ["integer", "null"]
            typeValue.isString shouldBe true
            typeValue.asString.get shouldBe "integer"
          case None =>
            fail("optionalField type not found in schema")

        // Verify requiredField is in required array but optionalField is not
        val requiredFields = inputSchema.hcursor.downField("required").as[List[String]].getOrElse(Nil)
        requiredFields should contain("requiredField")
        requiredFields should not contain "optionalField"
      case _ => fail("Expected Response")
