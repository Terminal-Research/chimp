package chimp.server

import io.circe.{Codec, Json}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.StatusCode
import sttp.monad.IdentityMonad
import sttp.tapir.Schema

class McpEndpointSpec extends AnyFlatSpec with Matchers:

  case class NoInput() derives Codec, Schema

  private val endpointTool = tool("endpointTest")
    .description("Endpoint test tool.")
    .input[NoInput]
    .handle(_ => Right("ok"))

  "mcpEndpoints" should "include POST handling and explicit GET rejection" in:
    val endpoints = mcpEndpoints(List(endpointTool), List("mcp"))
    val endpointSummary = endpoints.map(_.endpoint.show).mkString("\n")
    val getEndpoint = endpoints(1)
    val getResult =
      getEndpoint.logic(IdentityMonad)(
        ().asInstanceOf[getEndpoint.PRINCIPAL]
      )(().asInstanceOf[getEndpoint.INPUT])

    endpoints should have length 2
    endpointSummary should include("POST")
    endpointSummary should include("GET")
    getResult match
      case Right(output) =>
        val (status, body) =
          output.asInstanceOf[(StatusCode, Option[Json])]
        status shouldBe StatusCode.MethodNotAllowed
        body shouldBe empty
      case other => fail(s"Unexpected GET endpoint result: $other")
