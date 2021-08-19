package cats

// import cats.effect._
import cats.syntax.all._

import cats.effect.{IOApp, ExitCode, IO, Async}

import cats.Debug._

import scala.concurrent.duration._
import scala.util.Random

import cats.Functor
import cats.syntax.functor._

import cats.effect.kernel.{Ref, Deferred}
import cats.effect.std.{Semaphore, Supervisor, Queue}
import cats.effect.kernel.Sync
import cats.instances.list

//import cats.effect.concurrent.Deferred
//import cats.effect.{IO, Sync}
//import cats.effect.concurrent.Ref

import fs2._
import org.http4s._
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket._
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame._

// import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.global

import monocle.syntax.all._

object ParMapN extends IOApp {
  def run(args: List[String]): IO[ExitCode] = par.as(ExitCode.Success)

  val hello = IO("hello").debug
  val world = IO("world").debug
  val par =
    (hello, world)
      .parMapN((h, w) => s"$h $w")
      .debug
}

object ParMapNErrors extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    e1.attempt.debug *>
      IO("---").debug *>
      e2.attempt.debug *>
      IO("---").debug *>
      e3.attempt.debug *>
      IO.pure(ExitCode.Success)

  val ok = IO("OK").debug
  val error = IO.raiseError[String](new RuntimeException("ERROR")).debug
  val fail = IO.raiseError[String](new RuntimeException("FAIL")).debug

  val e1 = (ok, error).parMapN((_, _) => ())
  val e2 = (error, ok).parMapN((_, _) => ())
  val e3 = (error, fail).parMapN((_, _) => ())
}

object ParTupled extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    (ok, ko1).parTupled.void *>
      IO.pure(ExitCode.Success)

  val ok = IO("OK").debug
  val ko1 = IO.raiseError[String](new RuntimeException("ERROR")).debug
}

object ParTraverse extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    tasks
      .parTraverse(task)
      .debug
      .as(ExitCode.Success)
  val numTasks = 100
  val tasks: List[Int] = List.range(0, numTasks)
  def task(id: Int): IO[Int] = IO(id).debug
}

object FiberStart extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO("pre-first").debug
    fb1 <- IO(" first").debug.start
      _ <- IO("post-first").debug
      _ <- IO("pre-two").debug
    fb2 <- IO(" two").debug.start
      _ <- IO("post-two").debug
      _ <- IO("pre-join 1").debug
      _ <- fb1.join.debug
      _ <- IO("post-join 1").debug
      _ <- IO("pre-shift").debug
      //_ <- IO.shift
      _ <- IO("post-shift").debug
      _ <- IO("pre-join 2").debug
      _ <- fb2.join.debug
      _ <- IO("post-join 2").debug
    } yield ExitCode.Success
}

object ConcurrentStateRef extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      ticks <- Ref[IO].of(0L)
          _ <- (tickingClock(ticks), printTicks(ticks)).parTupled
    } yield ExitCode.Success
  def tickingClock(ticks: Ref[IO, Long]): IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis).debug
      _ <- ticks.update(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()
  def printTicks(ticks: Ref[IO, Long]): IO[Unit] =
    for {
      _ <- IO.sleep(5.seconds)
      n <- ticks.get
      _ <- IO(s"TICKS: $n").debug
      _ <- printTicks(ticks)
    } yield ()
}

object TwoRef extends IOApp {
  val mapIoRefStr = Map(1 -> Ref[IO].of(""))
  val ioListRefStr = mapIoRefStr.values.toList.sequence
  def run(args: List[String]): IO[ExitCode] = {
    for {
      listRefStr <- ioListRefStr
      listIoFsA = listRefStr.map(aTick(_).start)
      listIoFsB = listRefStr.map(bTick(_).start)
      listIoFsAB = listIoFsA ++ listIoFsB
      _ <- listIoFsAB.traverse(_.void)
      _ <- IO.never[ExitCode]
    } yield ExitCode.Success
  }

  def aTick(refStr: Ref[IO, String]): IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- refStr.update(_ + "a")
      _ <- refStr.get.debug
      _ <- aTick(refStr)
    } yield ()

  def bTick(refStr: Ref[IO, String]): IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- refStr.update(_ + "b")
      _ <- refStr.get.debug
      _ <- bTick(refStr)
    } yield ()
}

object Deferred13 extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      ticks <- Ref[IO].of(0L)
      is13 <- Deferred[IO, Unit]
      _ <- (beepWhen13(is13), tickingClock(ticks, is13)).parTupled
    } yield ExitCode.Success

  def beepWhen13(is13: Deferred[IO, Unit]) =
    for {
      _ <- is13.get
      _ <- IO("BEEP!").debug
    } yield ()

  def tickingClock(ticks: Ref[IO, Long], is13: Deferred[IO, Unit]): IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis).debug
      count <- ticks.updateAndGet(_ + 1)
      _ <- if (count >= 13) is13.complete(()) else IO.unit
      _ <- tickingClock(ticks, is13)
    } yield ()
}

object Regions extends IOApp.Simple {
  def randomSleep: IO[Unit] =
    IO(scala.util.Random.nextInt(100)).flatMap { ms =>
      IO.sleep((ms + 700).millis)
    }.void

  def p1(sem: Semaphore[IO]): IO[Unit] =
    sem.permit.surround(IO.println("Running P1")) >> randomSleep
  
  def p2(sem: Semaphore[IO]): IO[Unit] =
    sem.permit.surround(IO.println("Running P2")) >> randomSleep

  def run: IO[Unit] =
    Supervisor[IO].use { s =>
      Semaphore[IO](1).flatMap { sem =>
        s.supervise(p1(sem).foreverM).void *>
          s.supervise(p2(sem).foreverM).void *>
          IO.sleep(5.seconds).void
      }
    }
}

object RefCounter extends IOApp {
  def run(args: List[String]): IO[ExitCode] = LiveCounter.make[IO].flatMap(program(_)).as(ExitCode.Success)

  trait Counter[F[_]] {
    def incr: F[Unit]
    def get: F[Int]
  }

  import cats.Functor
  import cats.syntax.functor._

  object Counter {
    def make[F[_]: Functor: Ref.Make]: F[Counter[F]] =
      Ref.of[F, Int](0).map { ref =>
        new Counter[F] {
          def incr: F[Unit] = ref.update(_ + 1)
          def get: F[Int] = ref.get
        }
      }
  }

  object LiveCounter {
    def make[F[_]: Sync]: F[Counter[F]] =
      Ref.of[F, Int](0).map(new LiveCounter[F](_))
  }

  class LiveCounter[F[_]] private (
      ref: Ref[F, Int]
  ) extends Counter[F] {
    def incr: F[Unit] = ref.update(_ + 1)
    def get: F[Int] = ref.get
  }

  def program(c: Counter[IO]): IO[Unit] =
    for {
      _ <- c.get.flatMap(IO.println)
      _ <- c.incr
      _ <- c.get.flatMap(IO.println)
      _ <- c.incr.replicateA(5).void
      _ <- c.get.flatMap(IO.println)
    } yield ()
}

object ListRef extends IOApp {
  final case class Route()
  final case class User(id: Int, startFloor: Int, endFloor: Int)
  final case class Elevator(number: Int, floor: Int, route: Route, users: Set[User])

  val listIoRefE: List[IO[Ref[IO, Elevator]]] = List(
    Ref[IO].of(Elevator(1, 1, Route(), Set(User(1, 2, 3)))),
    Ref[IO].of(Elevator(2, 1, Route(), Set(User(2, 1, 2)))),
    Ref[IO].of(Elevator(3, 1, Route(), Set(User(3, 3, 1))))
  )

  val ioListE: IO[List[Elevator]] = listIoRefE.map(ioRefE => ioRefE.flatMap(refE => refE.get)).sequence

  def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- ioListE.debug
    } yield ExitCode.Success
  }
}

object MapMonoid extends App {
  final case class Ride(id: Int)
  final case class Elevator(id: Int)

  val e1 = Elevator(1)
  val e2 = Elevator(2)
  val e3 = Elevator(3)

  val r1 = Ride(1)
  val r2 = Ride(2)
  val r3 = Ride(3)
  val r4 = Ride(4)
  val r5 = Ride(5)

  val one = Map(
    e1 -> Set(r1),
    e2 -> Set(r2, r3),
  )

  val two = Map(
    e2 -> Set(r3, r4),
    e3 -> Set(r5),
  )

  def run(args: List[String]): Unit = {
    println(one |+| two)
  }
}

object BlazeWebSocketExample extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    BlazeWebSocketExampleApp[IO].stream.compile.drain.as(ExitCode.Success)
}

class BlazeWebSocketExampleApp[F[_]](implicit F: Async[F]) extends Http4sDsl[F] {
  def routes: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "wsecho" =>
        val echoReply: Pipe[F, WebSocketFrame, WebSocketFrame] =
          _.collect {
            case Text(msg, _) => Text("You sent the server: " + msg)
            case _ => Text("Something new")
          }

        Queue
          .unbounded[F, Option[WebSocketFrame]]
          .flatMap { q =>
            val to: Stream[F, WebSocketFrame] = Stream.fromQueueNoneTerminated(q).through(echoReply)
            val from: Pipe[F, WebSocketFrame, Unit] = _.enqueueNoneTerminated(q)
            WebSocketBuilder[F].build(to, from)
          }
    }

  def stream: Stream[F, ExitCode] =
    BlazeServerBuilder[F](global)
      .bindHttp(8080)
      .withHttpApp(routes.orNotFound)
      .serve
}

object BlazeWebSocketExampleApp {
  def apply[F[_]: Async]: BlazeWebSocketExampleApp[F] =
    new BlazeWebSocketExampleApp[F]
}