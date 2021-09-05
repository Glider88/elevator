package catz.application

import cats.syntax.all._
import cats.effect.IO
import cats.effect.std.Queue
import cats.effect.kernel.Ref
import io.circe.generic.auto._
import io.circe.syntax._
import scala.concurrent.duration._
import org.http4s.websocket.WebSocketFrame
// import Debug._
// import catz.domain.Shows._
import catz.domain.DomainCore._
import catz.domain.DomainElevator._
import catz.domain.DomainUser._

object AppMonoliteState {
  val FLOORS_NUMBER = 9
  val ELEVATORS_NUMBER = 3

  final case class AppState(floors: Map[Int, Floor], elevators: Map[Int, Elevator])

  val empty = Set.empty[User]
  val floorMap = (1 to FLOORS_NUMBER).map(n => n -> Floor(n, empty, empty)).toMap
  val elevatorMap = (1 to ELEVATORS_NUMBER).map(n => n -> Elevator.makeStart(n, 1.0)).toMap
  val appState = AppState(floorMap, elevatorMap)
  val setup = Request.mkSetup(ELEVATORS_NUMBER, FLOORS_NUMBER)
  val ioRefAppState = Ref[IO].of(appState)

  def fibers(id: IdGenerator[IO], refAppState: Ref[IO, AppState]) = {
    val floorFibers = (1 to FLOORS_NUMBER).map(newUserTick(id, _, refAppState).start)
    val elevatorFibers = (1 to ELEVATORS_NUMBER).map(elevatorTick(_, refAppState).start)
    (floorFibers ++ elevatorFibers).toList
  }

  def application(queue: Queue[IO, Option[WebSocketFrame]]): IO[Unit] = {
    for {
                _ <- queue.offer(WebSocketFrame.Text(setup.asJson.noSpaces).some)
               id <- LiveIdGenerator.make[IO]
      refAppState <- ioRefAppState
                _ <- fibers(id, refAppState).traverse(_.void)
                _ <- uiTick(queue, refAppState)
                _ <- IO.never[Unit]
    } yield ()
  }

  def uiTick(queue: Queue[IO, Option[WebSocketFrame]], refAppState: Ref[IO, AppState]): IO[Unit] = {
    for {
             _ <- IO.sleep(30.milliseconds)
      appState <- refAppState.get
                  request = Request.mkRequest(appState)
             _ <- queue.offer(WebSocketFrame.Text(request.asJson.noSpaces).some)
             _ <- uiTick(queue, refAppState)
    } yield ()
  }

  def newUserTick(Id: IdGenerator[IO], floorNumber: Int, refAppState: Ref[IO, AppState]): IO[Unit] =
    for {
             _ <- LiveRandomInt.nextInt(1, 30).flatMap(rnd => IO.sleep(rnd.seconds))
            id <- Id.nextId
      endFloor <- LiveRandomInt.nextInt(1, FLOORS_NUMBER + 1, floorNumber)
             _ <- refAppState.update(state => stateForNewUser(state, floorNumber, endFloor, id))
             _ <- newUserTick(Id, floorNumber, refAppState)
    } yield ()

  def elevatorTick(elevatorNumber: Int, refAppState: Ref[IO, AppState]): IO[Unit] =
    for {
      _ <- IO.sleep(30.milliseconds)
      // _ <- refAppState.get.show
      _ <- refAppState.update(state => stateForElevator(state, elevatorNumber))
      _ <- elevatorTick(elevatorNumber, refAppState)
    } yield ()

  def stateForNewUser(state: AppState, floorNumber: Int, endFloor: Int, id: Int): AppState = {
    val newRide = Ride(floorNumber, endFloor)
    val floors = state.floors.values.toList
    val elevators = state.elevators.values.toList
    val matchedElevator = matchElevator(3, 1)(floors, elevators, newRide)
    val newUser = User(id, newRide, Some(matchedElevator.number))
    val floor = state.floors(floorNumber)
    val updatedFloor = floor.copy(waiting = floor.waiting + newUser)
    val updatedElevator = matchedElevator.copy(route = addRide(matchedElevator.route, newRide))

    AppState(
      state.floors + (updatedFloor.number -> updatedFloor),
      state.elevators + (updatedElevator.number -> updatedElevator),
    )
  }

  def stateForElevator(state: AppState, elevatorNumber: Int): AppState = {
    val elevator = state.elevators(elevatorNumber)
    val movedElevator = elevatorMove(0.1, 10)(elevator)

    movedElevator.arrivedFloor match {
      case Some(floorNumber) => {
        val floor = state.floors(floorNumber)
        val unboardingUsers = movedElevator.users.filter(_.ride.end == floorNumber)
        val boardingUsers = floor.waiting.filter(
          _.matchedElevatorNumber match {
            case Some(matchedNumber) => matchedNumber == elevatorNumber
            case None => false
          }
        )

        val updatedFloor = floor.copy(
          waiting = floor.waiting -- boardingUsers,
          finished = floor.finished ++ unboardingUsers
        )

        val updatedElevator = movedElevator.copy(
          users = movedElevator.users -- unboardingUsers ++ boardingUsers
        )

        AppState(
          state.floors + (updatedFloor.number -> updatedFloor),
          state.elevators + (updatedElevator.number -> updatedElevator),
        )
      }
      case None => state.copy(elevators = state.elevators + (movedElevator.number -> movedElevator))
    }
  }
}
