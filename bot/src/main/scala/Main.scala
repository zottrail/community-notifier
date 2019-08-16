import java.nio.charset.StandardCharsets

import com.softwaremill.sttp._
import com.softwaremill.sttp.circe._
import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import pureconfig.generic.auto._

sealed trait PureConfigADT
case class RedditOAuthCredentials(clientId: String, clientSecret: String) extends PureConfigADT
case class RedditConfig(oauth: RedditOAuthCredentials, userAgent: String) extends PureConfigADT
case class Config(reddit: RedditConfig)

object Main {
  implicit val backend = HttpURLConnectionBackend()

  case class AccessTokenResponse(accessToken: String, expiresIn: Int)
  implicit val decodeAccessToken: Decoder[AccessTokenResponse] = new Decoder[AccessTokenResponse] {
    final def apply(c: HCursor): Decoder.Result[AccessTokenResponse] =
      for {
        accessToken <- c.downField("access_token").as[String]
        expiresIn <- c.downField("expires_in").as[Int]
      } yield {
        AccessTokenResponse(accessToken, expiresIn)
      }
  }

  case class RedditListingResponse(kind: String)
  implicit val decodeRedditListing: Decoder[RedditListingResponse] = new Decoder[RedditListingResponse] {
    final def apply(c: HCursor): Decoder.Result[RedditListingResponse] =
      for {
        kind <- c.downField("kind").as[String]
      } yield {
        RedditListingResponse(kind)
      }
  }

  def asBase64(str: String): String = java.util.Base64.getEncoder.encodeToString(str.getBytes(StandardCharsets.UTF_8))

  def main(args: Array[String]): Unit = {
    val response = for {
      config <- pureconfig.loadConfig[Config]

      request = sttp
        .post(uri"https://www.reddit.com/api/v1/access_token?grant_type=client_credentials")
        .headers(
          Map(
            "User-agent"     -> config.reddit.userAgent,
            "Authorization"  -> "Basic ".concat(
              asBase64(
                s"${config.reddit.oauth.clientId}:${config.reddit.oauth.clientSecret}"
              )
            )
          )
        )
        .body() // Avoid 411 (Content-Length: 0) errors.
        .response(asJson[AccessTokenResponse])

      response = request.send()
      body <- response.body
      accessTokenData <- body

      request = sttp
        .get(uri"https://oauth.reddit.com/r/uci/hot?limit=25")
        .headers(
          Map(
            "User-agent" -> config.reddit.userAgent,
            "Authorization" -> "Bearer ".concat(accessTokenData.accessToken)
          )
        )
        .response(asJson[RedditListingResponse])

      response = request.send()
      body <- response.body
      listingData <- body
    } yield listingData
    println("Response", response) // Left(error) or Right(response)
  }
}