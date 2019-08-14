import java.nio.charset.StandardCharsets

import pureconfig.generic.auto._

sealed trait PureConfigADT
case class RedditConfig(clientId: String, clientSecret: String) extends PureConfigADT
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
      case Right(r) => r.reddit // TODO: Add more return variables
    }

    val req = requests.post(
      "https://www.reddit.com/api/v1/access_token?grant_type=client_credentials",
      headers = Map(
        "Authorization" -> "Basic ".concat(asBase64(s"${config.clientId}:${config.clientSecret}")),
        "Content-Type"  -> "application/json"
      )
    )

    if (req.statusCode == 200) {
      val parsed = ujson.read(req.text).obj
      val accessToken = parsed("access_token").str
      val expiresIn = parsed("expires_in").num
      println(accessToken, expiresIn)
    } else {
      // TODO: throw error?
    }
  }
}