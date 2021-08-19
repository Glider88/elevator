package cats

import cats.syntax.all._

import cats.effect.{IOApp, ExitCode, IO, Async}
import cats.effect.std.Queue

import cats.Debug._

import scala.concurrent.duration._
import scala.util.Random

import cats.effect.kernel.Ref

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

import fs2._
import org.http4s._
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket._
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame._

import scala.concurrent.ExecutionContext.global


object ElevatorApp extends IOApp with Http4sDsl[IO] {
  import Domain._
  val floorMap = List.range(1, 10).map(n => n -> Floor(n, Set.empty[User], Set.empty[User])).toMap
  val elevatorMap = List.range(1, 4).map(n => n -> Elevator(n, 10, Route(), 0, false, Set.empty[User], None)).toMap

  val ioRefFloorMap = Ref[IO].of(floorMap)
  val ioRefElevatorMap = Ref[IO].of(elevatorMap)

  def routes: HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case GET -> Root / "ws" =>
        Queue
          .unbounded[IO, Option[WebSocketFrame]]
          .flatMap { q =>
            val to: Stream[IO, WebSocketFrame] = Stream.fromQueueNoneTerminated(q)
            val from: Pipe[IO, WebSocketFrame, Unit] = _.void

            app(q).start *> WebSocketBuilder[IO].build(to, from)
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

  def app(queue: Queue[IO, Option[WebSocketFrame]]): IO[Unit] = {
    for {
                   _ <- queue.offer(Text(SetupRequest(3, 9).asJson.noSpaces).some)
                  id <- LiveIdGenerator.make[IO]
         refFloorMap <- ioRefFloorMap
      refElevatorMap <- ioRefElevatorMap
                        floorFibers = List.range(1, 10).map(generateUserTick(id, _, refFloorMap, refElevatorMap).start)
                        elevatorFibers = List.range(1, 4).map(elevatorProcess(_, refFloorMap, refElevatorMap).start)
                        fibers = floorFibers ++ elevatorFibers
                   _ <- fibers.traverse(_.void)
                   _ <- uiTick(queue, refFloorMap, refElevatorMap)
                   _ <- IO.never[Unit]
    } yield ()
  }

  final case class StateRequest(elevators: Map[Int, ElevatorState], users: Map[Int, UserState], `type`: String = "tick")
  final case class SetupRequest(elevatorNumber: Int, floorNumber: Int, `type`: String = "setup")

  final case class UserState(from: Int, to: Int, status: String, elevator: Option[Int])
  final case class ElevatorState(floor: Int)

  def uiTick(
    queue: Queue[IO, Option[WebSocketFrame]],
    refFloorMap: Ref[IO, Map[Int, Floor]],
    refElevatorMap: Ref[IO, Map[Int, Elevator]]
  ): IO[Unit] = {
    for {
                _ <- IO.sleep(30.milliseconds)
         floorMap <- refFloorMap.get
      elevatorMap <- refElevatorMap.get
                    floorUserState = floorMap.foldLeft(Map.empty[Int, UserState]) {
                      case (acc, (_, floor)) => {
                        (acc
                         ++ (floor.waiting.map(u => u.id -> UserState(u.startFloor, u.endFloor, "waiting", u.matchedElevatorNumber)).toMap
                         ++ floor.finished.map(u => u.id -> UserState(u.startFloor, u.endFloor, "finished", u.matchedElevatorNumber)).toMap))
                      }
                    }

                    elevatorUserState = elevatorMap.foldLeft(Map.empty[Int, UserState]) {
                      case (acc, (_, elevator)) => {
                        acc ++ elevator.users.map(u => u.id -> UserState(u.startFloor, u.endFloor, "ride", u.matchedElevatorNumber))
                      }
                    }

                    stateRequest = StateRequest(
                      elevatorMap.view.mapValues(e => ElevatorState(e.floor)).toMap,
                      floorUserState ++ elevatorUserState
                    )
                _ <- queue.offer(Text(stateRequest.asJson.noSpaces).some)
                _ <- uiTick(queue, refFloorMap, refElevatorMap)
    } yield ()
  }

  def generateUserTick(
    Id: IdGenerator[IO],
    floorNumber: Int,
    refFloorMap: Ref[IO, Map[Int, Floor]],
    refElevatorMap: Ref[IO, Map[Int, Elevator]]
  ): IO[Unit] =
    for {
                   _ <- IO(Random.between(1, 30)).flatMap(rnd => IO.sleep(rnd.seconds))
                  id <- Id.nextId
            floorMap <- refFloorMap.get
         elevatorMap <- refElevatorMap.get
                        floor = floorMap(floorNumber)
      endFloorNumber <- LiveRandomIntExclude.nextInt(1, 10, floor.number)
                        newUser = User(id, floor.number, endFloorNumber, None)
                        newRide = Ride(floor.number * 10, endFloorNumber * 10)
                        newFloor = floor.copy(waiting = floor.waiting + newUser)
                   _ <- refFloorMap.update(fm => fm + (newFloor.number -> newFloor))
                        matchedElevator = matchElevator(floorMap.values.toList, elevatorMap.values.toList, newRide)
                        newUserWithElevator = newUser.copy(matchedElevatorNumber = Some(matchedElevator.number))
                        newFloorWithElevator = newFloor.copy(waiting = ((newFloor.waiting - newUser) + newUserWithElevator))
                        matchedElevatorUpdated = matchedElevator.copy(
                          route = matchedElevator.route.addRide(matchedElevator.floor, newRide)
                        )
                   _ <- refFloorMap.update(fm => fm + (newFloorWithElevator.number -> newFloorWithElevator))
                   _ <- refElevatorMap.update(em => em + (matchedElevatorUpdated.number -> matchedElevatorUpdated))
                   _ <- generateUserTick(Id, floorNumber, refFloorMap, refElevatorMap)
    } yield ()

  def elevatorProcess(
    elevatorNumber: Int,
    refFloorMap: Ref[IO, Map[Int, Floor]],
    refElevatorMap: Ref[IO, Map[Int, Elevator]]
  ): IO[Unit] =
    for {
                _ <- IO.sleep(30.milliseconds)
      elevatorMap <- refElevatorMap.get
                  elevator = elevatorMap(elevatorNumber)
                  updatedElevator = elevatorMove(elevator)
                _ <- refElevatorMap.update(elevatorMap => elevatorMap + (elevatorNumber -> updatedElevator))
         floorMap <- refFloorMap.get
                     (elevators, floors) = updatedElevator.arrivedFloor match {
                       case Some(floorNumber) =>
                         val unboardingUsers = updatedElevator.users.filter(_.endFloor == floorNumber)
                         val boardingUsers = floorMap(floorNumber).waiting.filter( u =>
                           u.matchedElevatorNumber.getOrElse(-1) * 10 != updatedElevator.number// && floorNumber == u.startFloor
                         )
                         val floor = floorMap(floorNumber)
                         val floors = Map(
                           floorNumber -> floor.copy(waiting = floor.waiting -- boardingUsers, finished = floor.finished ++ unboardingUsers)
                         )
                         val elevators = Map(
                           updatedElevator.number -> updatedElevator.copy(users = ((updatedElevator.users -- unboardingUsers) ++ boardingUsers))
                         )
                         (elevators, floors)
                       case None => (Map.empty[Int, Elevator], Map.empty[Int, Floor])
                     }
                _ <- refFloorMap.update(floorMap => floorMap ++ floors)
                _ <- refElevatorMap.update(elevatorMap => elevatorMap ++ elevators)
                _ <- refFloorMap.get.debug
                _ <- refElevatorMap.get.debug
                _ <- elevatorProcess(elevatorNumber, refFloorMap, refElevatorMap)
    } yield ()


  def matchElevator(floors: List[Floor], elevators: List[Elevator], newRide: Ride): Elevator = {
    val affectedRides = elevatorToAffectedRides(floors, elevators)

    val usersDiff = affectedRides.map { case (e, rides) => e -> affectedUsersTimeDiff(newRide, e, rides) }
    val newUserTimes = affectedRides.map { case (e, _) => e -> newUserTime(newRide, e) }

    val minUserTime = newUserTimes.minBy(_._2)._2
    val newUserDiff = newUserTimes.map { case (e, diffNewUser) => e -> (diffNewUser - minUserTime) }

    val allDiff = newUserDiff.map { case (e, diff) => (e, usersDiff(e) + diff) }
    allDiff.minBy(_._2)._1
  }

  def elevatorToAffectedRides(floors: List[Floor], elevators: List[Elevator]): Map[Elevator, Set[Ride]] = {
    val elevatorToPickedRides = elevators.map {
      e => e -> e.users.map(u => Ride(u.startFloor * 10, u.endFloor * 10))
    }.toMap

    val waitingUsers = floors.foldLeft(Set.empty[User])(_ ++ _.waiting)
    val waitingUserToElevatorNumber = waitingUsers.filter(_.matchedElevatorNumber.isDefined).map((u: User) => (u -> u.matchedElevatorNumber.get)).toMap
    val waitingUserToElevator = waitingUserToElevatorNumber.view.mapValues(n => elevators.find(e => e.number == n).get)
    val elevatorToMatchedRides = waitingUserToElevator.foldLeft(Map.empty[Elevator, Set[Ride]]) {
      (acc, u2e) => {
        val elevator = u2e._2
        val user = u2e._1
        acc.get(elevator) match {
          case Some(rides) => acc + (elevator -> (rides + Ride(user.startFloor * 10, user.endFloor * 10)))
          case None => acc + (elevator -> Set(Ride(user.startFloor * 10, user.endFloor * 10)))
        }
      }
    }

    elevatorToPickedRides |+| elevatorToMatchedRides
  }

  def newUserTime(newRide: Ride, elevator: Elevator): Int = {
    val route = elevator.route
    val currentFloor = elevator.floor * 10
    val potentialNewRoute = route.addRide(currentFloor, newRide)
    potentialNewRoute.time(currentFloor, newRide.end)
  }

  def affectedUsersTimeDiff(newRide: Ride, elevator: Elevator, affectedRides: Set[Ride]): Int = {
    val route = elevator.route
    val currentFloor = elevator.floor * 10
    val potentialNewRoute = route.addRide(currentFloor, newRide)
    val routeTime = affectedRides.foldLeft(0) ((allTime, ride) => allTime + route.time(currentFloor, ride.end))
    val potentialRouteTime = affectedRides.foldLeft(0) ((allTime, ride) => allTime + potentialNewRoute.time(currentFloor, ride.end))
    potentialRouteTime - routeTime
  }

  def elevatorMove(elevator: Elevator): Elevator = {
    val route = elevator.route
    val currentFloor = elevator.floor
    val stopTime = elevator.stopTime
    val afterStop = elevator.afterStop

    if (stopTime > 0) {
      elevator.copy(stopTime = stopTime - 1)
    } else {
      route.forward match {
        case Some(forwardWay) => {

          val restWay = forwardWay.restOfWay(currentFloor)
          val newCurrentFloor = if (forwardWay.direction == Up) currentFloor + 1 else currentFloor - 1

          if (restWay.floors.head == currentFloor) {
            if (afterStop) {
              if (forwardWay.restOfWay(newCurrentFloor).floors.isEmpty) {
                elevator.copy(
                  route = Route(forward = route.backward, backward = route.tail, tail = None),
                  floor = currentFloor,
                  stopTime = 0,
                  afterStop = false,
                  arrivedFloor = None
                )
              } else {
                elevator.copy(floor = newCurrentFloor , stopTime = 0, afterStop = false, arrivedFloor = None)
              }

            } else {
              elevator.copy(stopTime = 10, afterStop = true, arrivedFloor = Some(currentFloor / 10))
            }
          } else {
            elevator.copy(floor = newCurrentFloor , stopTime = 0)
          }
        }
        case None => {
          elevator.copy(stopTime = 0)
        }
      }
    }
  }
}
