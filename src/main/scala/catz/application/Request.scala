package catz.application

import catz.domain.DomainCore._
import AppMonoliteState.AppState

object Request {
  final case class SetupRequest(elevatorNumber: Int, floorNumber: Int, `type`: String = "setup")
  final case class StateRequest(elevators: Map[Int, ElevatorState], users: Map[Int, UserState], `type`: String = "tick")
  final case class UserState(from: Int, to: Int, status: String, elevator: Option[Int])
  final case class ElevatorState(floor: Int)

  object ElevatorState {
    def make(floor: Double): ElevatorState = ElevatorState((floor * 10).toInt)
  }

  def mkSetup(elevatorNumber: Int, floorNumber: Int): SetupRequest =
    SetupRequest(elevatorNumber, floorNumber)

  def mkRequest(floor: Floor): StateRequest = {
    val waiting = floor.waiting.map(
      u => u.id -> UserState(u.ride.start, u.ride.end, "waiting", u.matchedElevatorNumber)
    ).toMap

    val finished = floor.finished.map(
      u => u.id -> UserState(u.ride.start, u.ride.end, "finished", u.matchedElevatorNumber)
    ).toMap

    StateRequest(
      Map.empty[Int, ElevatorState],
      waiting ++ finished
    )
  }

  def mkRequest(elevator: Elevator): StateRequest = {
    val elevatorUsers = elevator.users.map(
      u => u.id -> UserState(u.ride.start, u.ride.end, "ride", u.matchedElevatorNumber)
    ).toMap

    StateRequest(
      Map(elevator.number -> ElevatorState.make(elevator.route.floor)),
      elevatorUsers
    )
  }

  def mkRequest(appState: AppState): StateRequest = {
    val floorUserState = appState.floors.foldLeft(Map.empty[Int, UserState]) {
      case (acc, (_, floor)) => {
        (acc
          ++ (floor.waiting.map(u => u.id -> UserState(u.ride.start, u.ride.end, "waiting", u.matchedElevatorNumber)).toMap
          ++ floor.finished.map(u => u.id -> UserState(u.ride.start, u.ride.end, "finished", u.matchedElevatorNumber)).toMap))
      }
    }

    val elevatorUserState = appState.elevators.foldLeft(Map.empty[Int, UserState]) {
      case (acc, (_, elevator)) => {
        acc ++ elevator.users.map(u => u.id -> UserState(u.ride.start, u.ride.end, "ride", u.matchedElevatorNumber))
      }
    }

    StateRequest(
      appState.elevators.view.mapValues(e => ElevatorState.make(e.route.floor)).toMap,
      floorUserState ++ elevatorUserState
    )
  }
}
