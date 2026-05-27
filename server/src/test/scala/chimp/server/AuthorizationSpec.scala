package chimp.server

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AuthorizationSpec extends AnyFlatSpec with Matchers:

  "ProtectedResourceMetadata" should "render protected resource metadata JSON" in:
    val metadata = ProtectedResourceMetadata(
      resource = "https://mcp.example.com/server/mcp",
      authorizationServers = List("https://auth.example.com"),
      scopesSupported = List("mcp:read"),
      resourceName = Some("Example MCP")
    )
    val json = metadata.asJson

    json.hcursor.downField("resource").as[String].toOption shouldBe
      Some("https://mcp.example.com/server/mcp")
    json.hcursor
      .downField("authorization_servers")
      .as[List[String]]
      .toOption shouldBe Some(List("https://auth.example.com"))
    json.hcursor.downField("bearer_methods_supported").as[List[String]].toOption shouldBe
      Some(List("header"))

  it should "derive well-known metadata URLs for path-addressed resources" in:
    ProtectedResourceMetadata.metadataUrlForResource(
      "https://mcp.example.com/server/mcp"
    ) shouldBe Right(
      "https://mcp.example.com/.well-known/oauth-protected-resource/server/mcp"
    )

  it should "reject invalid protected resource URIs" in:
    val error = ProtectedResourceMetadata
      .metadataUrlForResource(
        "https://mcp.example.com/server/mcp#fragment"
      )
      .left
      .toOption
      .getOrElse(fail("Expected invalid resource URI"))

    error should include("must not contain a fragment")

  "WWWAuthenticateChallenge" should "render a bearer metadata challenge" in:
    val challenge = WWWAuthenticateChallenge(
      resourceMetadata = "https://mcp.example.com/.well-known/oauth-protected-resource/server/mcp",
      error = Some("invalid_token"),
      errorDescription = Some("Token expired")
    )

    challenge.header.name shouldBe "WWW-Authenticate"
    challenge.header.value shouldBe
      "Bearer resource_metadata=\"" +
      "https://mcp.example.com/.well-known/oauth-protected-resource/server/mcp" +
      "\", error=\"invalid_token\", error_description=\"Token expired\""
