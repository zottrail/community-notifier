import java.nio.charset.StandardCharsets

import cats.{Monad}
import cats.effect.{ExitCode, IO, IOApp}
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.cats.AsyncHttpClientCatsBackend
import com.softwaremill.sttp.circe._
import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, HCursor}
import pureconfig.generic.auto._
import pureconfig.module.catseffect._

sealed trait PureConfigADT
case class RedditOAuthCredentials(clientId: String, clientSecret: String) extends PureConfigADT
case class RedditConfig(oauth: RedditOAuthCredentials, userAgent: String) extends PureConfigADT
case class Config(reddit: RedditConfig)

object Main extends IOApp {
  implicit val backend: SttpBackend[IO, Nothing] = AsyncHttpClientCatsBackend[IO]()

  trait RedditClient {
    def authorize(): IO[Either[Any, AccessTokenResponse]]
    def hot(accessToken: String): IO[Either[Any, RedditListingResponse]]
    def get(accessToken: String, id: String): IO[Either[Any, RedditPostResponse]]
    // def all(accessToken: String, id: List[String]): F[List[RedditPostResponse]]
  }

  class RedditClientInterpreter(val config: Config) extends RedditClient {
    def authorize(): IO[Either[Any, AccessTokenResponse]] = for {
      response <- sttp
      .post(uri"https://www.reddit.com/api/v1/access_token?grant_type=client_credentials")
      .headers(Map(withUserAgent, withBasicAuthorization))
      .body() // Avoid 411 (Content-Length: 0) errors.
      .response(asJson[AccessTokenResponse])
      .send()
      eitherToken <- extractRightFromResponse(response)
    } yield eitherToken

    override def hot(accessToken: String): IO[Either[Any, RedditListingResponse]] = for {
      response <- sttp
      .get(uri"https://oauth.reddit.com/r/uci/hot?limit=5")
      .headers(Map(withUserAgent, withBearerAuthorization(accessToken)))
      .response(asJson[RedditListingResponse])
      .send()
      eitherListing <- extractRightFromResponse(response)
    } yield eitherListing

    override def get(accessToken: String, id: String): IO[Either[Any, RedditPostResponse]] = for {
      response <- sttp
      .get(uri"https://oauth.reddit.com/r/uci/comments/$id")
      .headers(Map(withUserAgent, withBearerAuthorization(accessToken)))
      .response(asJson[RedditPostResponse])
      .send()
      eitherPost <- extractRightFromResponse(response)
    } yield eitherPost

    // override def all(accessToken: String, id: List[String]): F[List[RedditPostResponse]] = ???

    private def withUserAgent = "User-agent" -> config.reddit.userAgent

    private def withBasicAuthorization =
      "Authorization" -> "Basic ".concat(
        asBase64(
          s"${config.reddit.oauth.clientId}:${config.reddit.oauth.clientSecret}"
        )
      )

    private def withBearerAuthorization(accessToken: String)=
      "Authorization" -> "Bearer ".concat(accessToken)
  }

  object RedditClientInterpreter {
    def apply[F[_]: Monad](config: Config): RedditClientInterpreter =
      new RedditClientInterpreter(config)
  }

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

  case class RedditPostTitle(text: String, url: String)
  case class RedditPostTitleContainer(children: List[RedditPostTitle])
  implicit val decodeRedditPostTitle: Decoder[RedditPostTitle] = new Decoder[RedditPostTitle] {
    final def apply(c: HCursor): Decoder.Result[RedditPostTitle] =
      for {
        text <- c.downField("data").downField("selftext").as[String]
        url <- c.downField("data").downField("url").as[String]
      } yield {
        RedditPostTitle(text, url)
      }
  }
  implicit val decodeRedditPostTitleContainer: Decoder[RedditPostTitleContainer] = new Decoder[RedditPostTitleContainer] {
    final def apply(c: HCursor): Decoder.Result[RedditPostTitleContainer] = {
      for {
        children <- c.downField("data").downField("children").as[List[RedditPostTitle]]
      } yield {
        RedditPostTitleContainer(children)
      }
    }
  }

  case class RedditPostComment(body: String, permalink: String)
  case class RedditPostCommentContainer(children: List[RedditPostComment])
  implicit val decodeRedditPostComment: Decoder[RedditPostComment] = new Decoder[RedditPostComment] {
    final def apply(c: HCursor): Decoder.Result[RedditPostComment] =
      for {
        text <- c.downField("data").downField("body").as[String]
        permalink <- c.downField("data").downField("permalink").as[String]
      } yield {
        RedditPostComment(text, s"https://www.reddit.com$permalink")
      }
  }
  implicit val decodeRedditPostCommentContainer: Decoder[RedditPostCommentContainer] = new Decoder[RedditPostCommentContainer] {
    final def apply(c: HCursor): Decoder.Result[RedditPostCommentContainer] =
      for {
        children <- c.downField("data").downField("children").as[List[RedditPostComment]]
      } yield {
        RedditPostCommentContainer(children)
      }
  }

  case class RedditPostResponse(op: RedditPostTitle, comments: List[RedditPostComment])
  implicit val decodeRedditPost: Decoder[RedditPostResponse] = new Decoder[RedditPostResponse] {
    final def apply(c: HCursor): Decoder.Result[RedditPostResponse] =
      for {
        tuple <- c.as[(RedditPostTitleContainer, RedditPostCommentContainer)]
        op = tuple._1
        commentTree = tuple._2
      } yield {
        RedditPostResponse(op.children.head, commentTree.children)
      }
  }

  def asBase64(str: String): String = java.util.Base64.getEncoder.encodeToString(str.getBytes(StandardCharsets.UTF_8))

  val initializeConfig : IO[Config] = loadConfigF[IO, Config]

  def extractRightFromResponse[A](request: Response[Either[DeserializationError[Error], A]]) = IO {
    for {
      body <- request.body
      response <- body
    } yield response
  }

  // Should I make this use Either?
  // Something feels worng about this code... This will obviously throw exceptions,
  // but I'm unclear as to how I should make multiple Either's interact with multiple IO's.
  def doRedditIO(config: Config): IO[RedditPostResponse] = {
    val reddit = RedditClientInterpreter(config)
    for {
      eitherAccessTokenResponse <- reddit.authorize()
      accessTokenResponse = eitherAccessTokenResponse.right.get
      eitherListing <- reddit.hot(accessTokenResponse.accessToken)
      listing = eitherListing.right.get
      eitherPost <- reddit.get(accessTokenResponse.accessToken, listing.children.take(1).head.id)
      post = eitherPost.right.get
    } yield post
  }

  def run(args: List[String]): IO[ExitCode] = for {
    config <- initializeConfig
    post <- doRedditIO(config)
    _ = println(post.op.url)
    _ = println(post.comments.map(c => c.permalink))
  } yield ExitCode.Success
}