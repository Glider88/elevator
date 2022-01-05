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
  def routes(application: Queue[IO,Option[WebSocketFrame]] => IO[Unit]): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case GET -> Root / "ws" =>
        Queue
          .unbounded[IO, Option[WebSocketFrame]]
          .flatMap { queue =>
            val to: Stream[IO, WebSocketFrame] = Stream.fromQueueNoneTerminated(queue)
            val from: Pipe[IO, WebSocketFrame, Unit] = _.void
            
            application(queue).start *> WebSocketBuilder[IO].build(to, from)
          }
    }

  def run(args: List[String]): IO[ExitCode] = {
    val application = args match {
      case "monolite" :: Nil => queue => catz.application.AppMonoliteState.application(queue)
      case "separate" :: Nil => queue => catz.application.AppSeparateState.application(queue)
      case _ => throw new IllegalArgumentException("expected argument 'monolite' or 'separate'")
    }

    BlazeServerBuilder[IO](global)
      .bindHttp(12346)
      .withHttpApp(routes(application).orNotFound)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
