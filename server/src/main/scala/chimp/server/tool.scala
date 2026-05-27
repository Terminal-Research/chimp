package chimp.server

import chimp.protocol.{CallToolResult, ToolContent}
import sttp.tapir.Schema
import io.circe.{Decoder, Json}
import sttp.model.Header
import sttp.shared.Identity

case class ToolAnnotations(
    title: Option[String] = None,
    readOnlyHint: Option[Boolean] = None,
    destructiveHint: Option[Boolean] = None,
    idempotentHint: Option[Boolean] = None,
    openWorldHint: Option[Boolean] = None
)

/** Describes a tool before the input is specified. */
case class PartialTool(
    name: String,
    description: Option[String] = None,
    annotations: Option[ToolAnnotations] = None,
    outputSchema: Option[Schema[?]] = None
):
  def description(desc: String): PartialTool = copy(description = Some(desc))
  def withAnnotations(ann: ToolAnnotations): PartialTool = copy(annotations = Some(ann))
  def output[O: Schema]: PartialTool =
    copy(outputSchema = Some(summon[Schema[O]]))

  /** Specify the input type for the tool, providing both a Tapir Schema and a Circe Decoder. */
  def input[I: Schema: Decoder]: Tool[I] =
    Tool[I](
      name,
      description,
      summon[Schema[I]],
      summon[Decoder[I]],
      outputSchema,
      annotations
    )

private val ToolNameRegex = "^[A-Za-z0-9_./-]+$".r

/** Creates a new MCP tool description with the given name. The name must match `^[A-Za-z0-9_./-]+$` and be 1–64 characters long. */
def tool(name: String): PartialTool =
  require(name.length >= 1 && name.length <= 64, s"Tool name must be 1..64 characters long, got ${name.length}: $name")
  require(ToolNameRegex.matches(name), s"Tool name must match ${ToolNameRegex.regex}, got: $name")
  PartialTool(name)

//

/** Describes a tool after the input is specified. */
case class Tool[I](
    name: String,
    description: Option[String],
    inputSchema: Schema[I],
    inputDecoder: Decoder[I],
    outputSchema: Option[Schema[?]],
    annotations: Option[ToolAnnotations]
):
  def output[O: Schema]: Tool[I] =
    copy(outputSchema = Some(summon[Schema[O]]))

  /** Combine the tool description with the server logic, that should be executed when the tool is invoked. The logic, given the input,
    * should return either a tool execution error (`Left`), or a successful textual result (`Right`), using the F-effect.
    */
  def serverLogic[F[_]](logic: (I, Seq[Header]) => F[Either[String, String]]): ServerTool[I, F] =
    ServerTool(
      name,
      description,
      inputSchema,
      inputDecoder,
      outputSchema,
      annotations,
      ServerToolLogic.Text(logic)
    )

  /** Combine the tool description with structured-output server logic, using the F-effect. */
  def serverOutputLogic[F[_]](
      logic: (I, Seq[Header]) => F[ToolOutput]
  ): ServerTool[I, F] =
    ServerTool(
      name,
      description,
      inputSchema,
      inputDecoder,
      outputSchema,
      annotations,
      ServerToolLogic.Output(logic)
    )

  /** Combine the tool description with the server logic, that should be executed when the tool is invoked. The logic, given the input,
    * should return either a tool execution error (`Left`), or a successful textual result (`Right`).
    *
    * Same as [[serverLogic]], but using the identity "effect".
    */
  def handleWithHeaders(logic: (I, Seq[Header]) => Either[String, String]): ServerTool[I, Identity] =
    ServerTool(
      name,
      description,
      inputSchema,
      inputDecoder,
      outputSchema,
      annotations,
      ServerToolLogic.Text((i, t) => logic(i, t))
    )

  /** Combine the tool description with structured-output server logic. */
  def handleOutputWithHeaders(
      logic: (I, Seq[Header]) => ToolOutput
  ): ServerTool[I, Identity] =
    ServerTool(
      name,
      description,
      inputSchema,
      inputDecoder,
      outputSchema,
      annotations,
      ServerToolLogic.Output((i, t) => logic(i, t))
    )

  /** Combine the tool description with the server logic, that should be executed when the tool is invoked. The logic, given the input,
    * should return either a tool execution error (`Left`), or a successful textual result (`Right`).
    *
    * Same as [[handleWithHeaders]], but using no headers.
    */
  def handle(logic: I => Either[String, String]): ServerTool[I, Identity] =
    handleWithHeaders((i, _) => logic(i))

  /** Combine the tool description with structured-output server logic. */
  def handleOutput(logic: I => ToolOutput): ServerTool[I, Identity] =
    handleOutputWithHeaders((i, _) => logic(i))

/** A structured or textual tool result emitted by server logic. */
case class ToolOutput(
    content: List[ToolContent],
    structuredContent: Option[Json] = None,
    isError: Boolean = false,
    meta: Option[Map[String, Json]] = None
):
  def toCallToolResult: CallToolResult =
    CallToolResult(content, structuredContent, isError, meta)

object ToolOutput:
  def text(text: String): ToolOutput =
    ToolOutput(List(ToolContent.Text(text = text)))

  def error(text: String): ToolOutput =
    ToolOutput(List(ToolContent.Text(text = text)), isError = true)

  def structured(
      content: List[ToolContent],
      structuredContent: Json
  ): ToolOutput =
    ToolOutput(content, structuredContent = Some(structuredContent))

enum ServerToolLogic[I, F[_]]:
  case Text(logic: (I, Seq[Header]) => F[Either[String, String]])
  case Output(logic: (I, Seq[Header]) => F[ToolOutput])

/** A tool that can be executed by the MCP server. */
case class ServerTool[I, F[_]](
    name: String,
    description: Option[String],
    inputSchema: Schema[I],
    inputDecoder: Decoder[I],
    outputSchema: Option[Schema[?]],
    annotations: Option[ToolAnnotations],
    logic: ServerToolLogic[I, F]
)
