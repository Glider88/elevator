package catz

import cats.syntax.all._
import cats.effect.{IOApp, ExitCode, IO}
import cats.effect.std.Queue
import fs2._
import org.http4s._
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket._
import org.http4s.websocket.WebSocketFrame
import scala.concurrent.ExecutionContext.global

object Main extends IOApp with Http4sDsl[IO] {
  def routes: HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case GET -> Root / "ws" =>
        Queue
          .unbounded[IO, Option[WebSocketFrame]]
          .flatMap { queue =>
            val to: Stream[IO, WebSocketFrame] = Stream.fromQueueNoneTerminated(queue)
            val from: Pipe[IO, WebSocketFrame, Unit] = _.void

            app(queue).start *> WebSocketBuilder[IO].build(to, from)
          }
    }

  def run(args: List[String]): IO[ExitCode] = {
    BlazeServerBuilder[IO](global)
      .bindHttp(12346)
      .withHttpApp(routes.orNotFound)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }

  def app(queue: Queue[IO, Option[WebSocketFrame]]) = catz.application.AppMonoliteState.application(queue)
  // def app(queue: Queue[IO, Option[WebSocketFrame]]) = catz.application.AppSeparateState.application(queue)
}
