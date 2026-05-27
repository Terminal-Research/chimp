package chimp.server

import io.circe.Json
import io.circe.syntax.*
import sttp.model.Header

import java.net.URI

/** OAuth protected-resource metadata for an HTTP MCP endpoint. */
final case class ProtectedResourceMetadata(
    resource: String,
    authorizationServers: List[String],
    scopesSupported: List[String] = Nil,
    bearerMethodsSupported: List[String] = List("header"),
    resourceName: Option[String] = None,
    resourceDocumentation: Option[String] = None
):
  require(
    authorizationServers.nonEmpty,
    "Protected resource metadata requires at least one authorization server"
  )

  def asJson: Json =
    val fields = List(
      Some("resource" -> Json.fromString(resource)),
      Some("authorization_servers" -> authorizationServers.asJson),
      Option.when(scopesSupported.nonEmpty)(
        "scopes_supported" -> scopesSupported.asJson
      ),
      Option.when(bearerMethodsSupported.nonEmpty)(
        "bearer_methods_supported" -> bearerMethodsSupported.asJson
      ),
      resourceName.map("resource_name" -> Json.fromString(_)),
      resourceDocumentation.map(
        "resource_documentation" -> Json.fromString(_)
      )
    )
    Json.obj(fields.flatten*)

/** Challenge data for a `WWW-Authenticate` response header. */
final case class WWWAuthenticateChallenge(
    resourceMetadata: String,
    realm: Option[String] = None,
    error: Option[String] = None,
    errorDescription: Option[String] = None,
    scope: Option[String] = None
):
  def header: Header =
    Header("WWW-Authenticate", headerValue)

  def headerValue: String =
    val params = List(
      Some("resource_metadata" -> resourceMetadata),
      realm.map("realm" -> _),
      error.map("error" -> _),
      errorDescription.map("error_description" -> _),
      scope.map("scope" -> _)
    )
    "Bearer " + params.flatten.map(formatParam).mkString(", ")

  private def formatParam(param: (String, String)): String =
    val (name, value) = param
    s"""$name="${escape(value)}""""

  private def escape(value: String): String =
    value.flatMap:
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case c    => c.toString

object ProtectedResourceMetadata:
  val WellKnownPath: String = "/.well-known/oauth-protected-resource"

  def metadataUrlForResource(resource: String): Either[String, String] =
    parseResourceUri(resource).map: uri =>
      val path = Option(uri.getRawPath).getOrElse("")
      val query = Option(uri.getRawQuery).fold("")(value => s"?$value")
      val metadataPath =
        if path.isEmpty || path == "/" then WellKnownPath
        else WellKnownPath + path
      s"${uri.getScheme.toLowerCase}://${uri.getRawAuthority}$metadataPath$query"

  private def parseResourceUri(resource: String): Either[String, URI] =
    try
      val uri = URI(resource)
      if uri.getScheme == null || uri.getRawAuthority == null then Left(s"Protected resource URI must be absolute: $resource")
      else if uri.getRawFragment != null then Left(s"Protected resource URI must not contain a fragment: $resource")
      else if uri.getRawUserInfo != null then Left(s"Protected resource URI must not contain userinfo: $resource")
      else Right(uri)
    catch
      case ex: IllegalArgumentException =>
        Left(s"Invalid protected resource URI: ${ex.getMessage}")
