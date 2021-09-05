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
import catz.domain.DomainUser._
import catz.domain.DomainCore._
import catz.domain.DomainElevator._
import catz.domain.DoubleComparison._

object AppSeparateState {
  val FLOORS_NUMBER = 9
  val ELEVATORS_NUMBER = 3

  val empty = Set.empty[User]
  val ioRefFloors = (1 to FLOORS_NUMBER).map(n => Ref[IO].of(Floor(n, empty, empty))).toList.sequence
  val ioRefElevators = (1 to ELEVATORS_NUMBER).map(n => Ref[IO].of(Elevator.makeStart(n, 1.0))).toList.sequence
  val setup = Request.mkSetup(ELEVATORS_NUMBER, FLOORS_NUMBER)

  def fibers(
    id: IdGenerator[IO],
    refElevators: List[Ref[IO, Elevator]],
    refFloors: List[Ref[IO, Floor]],
    queue: Queue[IO, Option[WebSocketFrame]]
  ) = {
    val uiFloor = refFloors.map(uiFloorTick(queue, _).start)
    val uiElevator = refElevators.map(uiElevatorTick(queue, _).start)
    val newUser = refFloors.map(newUserTick(id, _).start)
    val matching = refFloors.map(matchingElevatorTick(id, refElevators, refFloors, _).start)
    val moving = refElevators.map(elevatorMovingTick(_).start)
    val boarding = refElevators.map(boardingTick(refFloors, _).start)
    val unboarding = refElevators.map(unboardingTick(refFloors, _).start)

    (uiFloor ++ uiElevator ++ newUser ++ matching ++ moving ++ boarding ++ unboarding).toList
  }

  def snapshot[A](listRef: List[Ref[IO, A]]): IO[List[A]] = listRef.map(ref => ref.get).sequence

  def application(queue: Queue[IO, Option[WebSocketFrame]]): IO[Unit] = {
    for {
                 _ <- queue.offer(WebSocketFrame.Text(setup.asJson.noSpaces).some)
                id <- LiveIdGenerator.make[IO]
         refFloors <- ioRefFloors
      refElevators <- ioRefElevators
                 _ <- fibers(id, refElevators, refFloors, queue).traverse(_.void)
                 _ <- IO.never[Unit]
    } yield ()
  }

  def uiFloorTick(queue: Queue[IO, Option[WebSocketFrame]], refFloor: Ref[IO, Floor]): IO[Unit] = {
    for {
          _ <- IO.sleep(30.milliseconds)
      floor <- refFloor.get
               request = Request.mkRequest(floor)
          _ <- queue.offer(WebSocketFrame.Text(request.asJson.noSpaces).some)
          _ <- uiFloorTick(queue, refFloor)
    } yield ()
  }

  def uiElevatorTick(queue: Queue[IO, Option[WebSocketFrame]], refElevator: Ref[IO, Elevator]): IO[Unit] = {
    for {
             _ <- IO.sleep(30.milliseconds)
      elevator <- refElevator.get
                  request = Request.mkRequest(elevator)
             _ <- queue.offer(WebSocketFrame.Text(request.asJson.noSpaces).some)
             _ <- uiElevatorTick(queue, refElevator)
    } yield ()
  }

  def newUserTick(Id: IdGenerator[IO], refFloor: Ref[IO, Floor]): IO[Unit] =
    for {
             _ <- LiveRandomInt.nextInt(1, 30).flatMap(rnd => IO.sleep(rnd.seconds))
            id <- Id.nextId
         floor <- refFloor.get
      endFloor <- LiveRandomInt.nextInt(1, FLOORS_NUMBER + 1, floor.number)
             _ <- refFloor.update(floor =>
                    floor.copy(waiting = floor.waiting + User(id, Ride(floor.number, endFloor), None))
                  )
             _ <- newUserTick(Id, refFloor)
    } yield ()

  def matchingElevatorTick(
    Id: IdGenerator[IO],
    refElevators: List[Ref[IO, Elevator]],
    refFloors: List[Ref[IO, Floor]],
    refFloor: Ref[IO, Floor]
  ): IO[Unit] =
    for {
              _ <- IO.sleep(30.milliseconds)
              // _ <- refFloor.get.show
      elevators <- snapshot(refElevators)
      floors    <- snapshot(refFloors)
      floor     <- refFloor.get
                   consistentElevators = filterUnsafe(elevators, floor)
                   waitingUserOption = floor.waiting.find(_.matchedElevatorNumber.isEmpty)
                   matchedElevatorOption = waitingUserOption.map(u =>
                     matchElevator(3, 1)(floors, consistentElevators, u.ride)
                   )
                   refMatchedElevatorOption = matchedElevatorOption.map(e => refElevators(e.number - 1))
              _ <- refFloor.update(floor => updateFloorMatching(floor, waitingUserOption, matchedElevatorOption))
              _ <- refMatchedElevatorOption match {
                      case None => IO.unit
                      case Some(refMatchedElevator) => {
                        refMatchedElevator.update(matched => updateElevatorMatching(matched, waitingUserOption))
                      }
                    }
              _ <- matchingElevatorTick(Id, refElevators, refFloors, refFloor)
    } yield ()

  def elevatorMovingTick(refElevator: Ref[IO, Elevator]): IO[Unit] =
    for {
      _ <- IO.sleep(30.milliseconds)
      // _ <- refElevator.get.show
      _ <- refElevator.update(elevatorMove(0.1, 10)(_))
      _ <- elevatorMovingTick(refElevator)
    } yield ()

  def boardingTick(
    refFloors: List[Ref[IO, Floor]],
    refElevator: Ref[IO, Elevator]
  ): IO[Unit] =
    for {
             _ <- IO.sleep(30.milliseconds)
        floors <- snapshot(refFloors)
             _ <- refElevator.update(elevator => boardingElevator(elevator, floors))
      elevator <- refElevator.get
                  refFloorOption = elevator.arrivedFloor.map(floor => refFloors(floor - 1))
             _ <- refFloorOption match {
                    case None => IO.unit
                    case Some(refFloor) => {
                      refFloor.update(floor => boardingFloor(elevator, floor))
                    }
                  }
             _ <- boardingTick(refFloors, refElevator)
    } yield ()

  def unboardingTick(
    refFloors: List[Ref[IO, Floor]],
    refElevator: Ref[IO, Elevator]
  ): IO[Unit] =
    for {
             _ <- IO.sleep(30.milliseconds)
      elevator <- refElevator.get
                  refFloorOption = elevator.arrivedFloor.map(floor => refFloors(floor - 1))
             _ <- refFloorOption match {
                    case None => IO.unit
                    case Some(refFloor) => {
                      refFloor.update(floor => unboardingFloor(elevator, floor))
                    }
                  }
             _ <- refElevator.update(elevator => unboardingElevator(elevator))
             _ <- unboardingTick(refFloors, refElevator)
    } yield ()

  def filterUnsafe(elevators: List[Elevator], currentFloor: Floor): List[Elevator] =
    elevators
      .filter(e => e.stopTime >= 5 || (e.route.floor neq currentFloor.number))
      .filter(e =>
        e.route.forward match {
          case None => true
          case Some(forwardWay) => {
            if (forwardWay.direction == Up) {
              (currentFloor.number - e.route.floor) gt 0.3
            } else {
              (e.route.floor - currentFloor.number) gt 0.3
            }
          }
        }
      )

  def updateFloorMatching(floor: Floor, waiting: Option[User], matched: Option[Elevator]): Floor =
    (waiting, matched) match {
      case (Some(user), Some(elevator)) => {
        val updatedUser = user.copy(matchedElevatorNumber = Some(elevator.number))
        floor.copy(waiting = floor.waiting - user + updatedUser)
      }
      case _ => floor
    }

  def updateElevatorMatching(elevator: Elevator, waiting: Option[User]): Elevator =
    waiting match {
      case None => elevator
      case Some(user) => elevator.copy(
        route = addRide(elevator.route, user.ride)
      )
    }

  def unboardingElevator(elevator: Elevator): Elevator =
    elevator.arrivedFloor match {
      case Some(floorNumber) => {
        elevator.copy(users = elevator.users -- unboardingUsers(elevator, floorNumber))
      }
      case None => elevator
    }

  def unboardingFloor(elevator: Elevator, floor: Floor): Floor =
    floor.copy(finished = floor.finished ++ unboardingUsers(elevator, floor.number))

  def unboardingUsers(elevator: Elevator, floorNumber: Int): Set[User] =
    elevator.users.filter(_.ride.end == floorNumber)

  def boardingElevator(elevator: Elevator, floors: List[Floor]): Elevator =
    elevator.arrivedFloor match {
      case Some(floorNumber) => {
        val floor = floors(floorNumber - 1)
        elevator.copy(users = elevator.users ++ boardingUsers(elevator, floor))
      }
      case None => elevator
    }

  def boardingFloor(elevator: Elevator, floor: Floor): Floor =
    floor.copy(waiting = floor.waiting -- boardingUsers(elevator, floor))

  def boardingUsers(elevator: Elevator, floor: Floor): Set[User] =
    floor.waiting.filter(
      _.matchedElevatorNumber match {
        case Some(matchedNumber) => matchedNumber == elevator.number
        case None => false
      }
    )
}
