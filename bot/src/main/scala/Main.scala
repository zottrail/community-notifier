import java.nio.charset.StandardCharsets

import cats.effect.{ContextShift, ExitCode, IO, IOApp}
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.cats.AsyncHttpClientCatsBackend
import com.softwaremill.sttp.circe._

import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

import pureconfig.generic.auto._
import pureconfig.module.catseffect._

import scala.concurrent.ExecutionContext.Implicits.global

sealed trait PureConfigADT
case class RedditOAuthCredentials(clientId: String, clientSecret: String) extends PureConfigADT
case class RedditConfig(oauth: RedditOAuthCredentials, userAgent: String) extends PureConfigADT
case class Config(reddit: RedditConfig)

object Main extends IOApp {
  implicit val backend: SttpBackend[IO, Nothing] = AsyncHttpClientCatsBackend[IO]()

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

  case class RedditListingItem(id: String)
  case class RedditListingItemContainer(data: RedditListingItem)
  case class RedditListingResponse(children: List[RedditListingItem])
  implicit val decodeRedditListing: Decoder[RedditListingResponse] = new Decoder[RedditListingResponse] {
    final def apply(c: HCursor): Decoder.Result[RedditListingResponse] =
      for {
        children <- c.downField("data").downField("children").as[List[RedditListingItemContainer]]
      } yield {
        RedditListingResponse(children.map(_.data))
      }
  }

  def asBase64(str: String): String = java.util.Base64.getEncoder.encodeToString(str.getBytes(StandardCharsets.UTF_8))

  val initializeConfig : IO[Config] = loadConfigF[IO, Config]

  def run(args: List[String]): IO[ExitCode] = for {
    config <- initializeConfig
    accessTokenRequest <- sttp
      .post(uri"https://www.reddit.com/api/v1/access_token?grant_type=client_credentials")
      .headers(
        Map(
          "User-agent" -> config.reddit.userAgent,
          "Authorization" -> "Basic ".concat(
            asBase64(
              s"${config.reddit.oauth.clientId}:${config.reddit.oauth.clientSecret}"
            )
          )
        )
      )
      .body() // Avoid 411 (Content-Length: 0) errors.
      .response(asJson[AccessTokenResponse])
      .send()
    accessTokenResponse <- IO {
      for {
        accessTokenBody <- accessTokenRequest.body
        accessTokenResponse <- accessTokenBody.right
      } yield accessTokenResponse
    }
    redditListingRequest <- sttp
      .get(uri"https://oauth.reddit.com/r/uci/hot?limit=25")
      .headers(
        Map(
          "User-agent" -> config.reddit.userAgent,
          "Authorization" -> "Bearer ".concat(accessTokenResponse.right.get.accessToken)
        )
      )
      .response(asJson[RedditListingResponse])
      .send()
    redditListingResponse <- IO {
      for {
        redditListingBody <- redditListingRequest.body
        redditListingResponse <- redditListingBody.right
      } yield redditListingResponse
    }
    _ = println(redditListingResponse.right.get)
  } yield ExitCode.Success
}