package chimp.server

import io.circe.{Codec, Json}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.{Header, StatusCode}
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
      )(Seq.empty[Header].asInstanceOf[getEndpoint.INPUT])

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

  it should "reject disallowed Origin headers" in:
    val endpoints = mcpEndpoints(
      List(endpointTool),
      List("mcp"),
      originPolicy = OriginPolicy.allowOnly(Set("https://allowed.example"))
    )
    val getEndpoint = endpoints(1)
    val getResult =
      getEndpoint.logic(IdentityMonad)(
        ().asInstanceOf[getEndpoint.PRINCIPAL]
      )(
        Seq(Header("Origin", "https://blocked.example"))
          .asInstanceOf[getEndpoint.INPUT]
      )

    getResult match
      case Right(output) =>
        val (status, body) =
          output.asInstanceOf[(StatusCode, Option[Json])]
        status shouldBe StatusCode.Forbidden
        body.flatMap(_.hcursor.downField("error").as[String].toOption) shouldBe
          Some("Origin is not allowed: https://blocked.example")
      case other => fail(s"Unexpected GET endpoint result: $other")

  "OriginPolicy" should "allow requests with configured origins" in:
    val policy = OriginPolicy.allowOnly(Set("https://allowed.example"))

    policy.validate(
      Seq(Header("Origin", "https://allowed.example"))
    ) shouldBe Right(())
