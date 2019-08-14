import java.nio.charset.StandardCharsets

import com.softwaremill.sttp._
import com.softwaremill.sttp.circe._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import io.circe.{ Decoder, Encoder, HCursor, Json }
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

  def asBase64(str: String): String = java.util.Base64.getEncoder.encodeToString(str.getBytes(StandardCharsets.UTF_8))

  def main(args: Array[String]): Unit = {
    val config = pureconfig.loadConfig[Config] match {
      // TODO: What should I do here for proper functional programming?
      case Left(l) => {
        println(l)
        throw new IllegalArgumentException("could not find credentials. try client-id / client-secret.")
      }
      case Right(r) => r // TODO: Add more return variables.
    }

    val request = sttp
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

    val response = request.send()
    val body = response.unsafeBody

    body match {
      case Left(l) => {
        println("decodable error:", l)
      }
      case Right(r) => {
        println(r.accessToken, r.expiresIn)
      }
    }
  }
}