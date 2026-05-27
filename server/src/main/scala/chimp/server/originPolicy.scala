package chimp.server

import sttp.model.Header

/** Origin validation policy for MCP Streamable HTTP requests. */
enum OriginPolicy:
  case AllowAll
  case RejectBrowserOrigins
  case AllowOnly(origins: Set[String])

  def validate(headers: Seq[Header]): Either[String, Unit] =
    val origins = headers
      .collect:
        case header if header.name.equalsIgnoreCase("Origin") =>
          header.value.trim
      .distinct
    origins match
      case Nil =>
        Right(())
      case origin :: Nil =>
        validateOrigin(origin)
      case _ =>
        Left("Conflicting Origin headers")

  private def validateOrigin(origin: String): Either[String, Unit] =
    this match
      case AllowAll =>
        Right(())
      case RejectBrowserOrigins =>
        Left(s"Origin is not allowed: $origin")
      case AllowOnly(origins) if origins(origin) =>
        Right(())
      case AllowOnly(_) =>
        Left(s"Origin is not allowed: $origin")

object OriginPolicy:
  def allowOnly(origins: Set[String]): OriginPolicy =
    OriginPolicy.AllowOnly(origins)
