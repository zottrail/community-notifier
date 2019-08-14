import java.nio.charset.StandardCharsets

import pureconfig.generic.auto._
import com.softwaremill.sttp._

sealed trait PureConfigADT
case class RedditOAuthCredentials(clientId: String, clientSecret: String) extends PureConfigADT
case class RedditConfig(oauth: RedditOAuthCredentials, userAgent: String) extends PureConfigADT
case class Config(reddit: RedditConfig)

object Main {
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

    val req = sttp
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

    implicit val backend = HttpURLConnectionBackend()
    val res = req.send()

    if (res.code == 200) {
      val parsed = ujson.read(res.unsafeBody).obj
      val accessToken = parsed("access_token").str
      val expiresIn = parsed("expires_in").num
      println(accessToken, expiresIn)
    } else {
      // TODO: throw error?
    }
  }
}