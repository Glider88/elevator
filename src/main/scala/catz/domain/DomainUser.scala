package catz.domain

import DomainCore._
import DoubleComparison._
import DomainElevator.addRide

object DomainUser {
  def matchElevator(stopTime: Int, pathTime: Int)(floors: List[Floor], elevators: List[Elevator], newRide: Ride): Elevator = {
    val time = routeTime(stopTime, pathTime) _
    val elevatorToRouteTime = elevators.map {
      e => e -> time(e.route, e.users.map(_.ride), affectedRides(e.number, floors))
    }.toMap

    val elevatorToNewRouteTime = elevators.map {
      e => e -> time(addRide(e.route, newRide), e.users.map(_.ride), affectedRides(e.number, floors) ++ Set(newRide))
    }.toMap

    val allTime = elevatorToRouteTime.values.sum
    val elevatorToAllTime = elevators.map(e => e -> (allTime - elevatorToRouteTime(e) + elevatorToNewRouteTime(e)))

    elevatorToAllTime.minBy(_._2)._1
  }

  def affectedRides(elevatorNumber: Int, floors: List[Floor]): Set[Ride] = {
    val waitingUsers = floors.foldLeft(Set.empty[User])(_ ++ _.waiting)
    val waitingElevatorUsers = waitingUsers.filter { user =>
      user.matchedElevatorNumber match {
        case Some(matchedElevatorNumber) => matchedElevatorNumber == elevatorNumber
        case None => false
      }
    }

    waitingElevatorUsers.map(_.ride)
  }

  def routeTime(stopTime: Int, pathTime: Int)(
    route: Route,
    elevatorRides: Set[Ride],
    floorAffectedRides: Set[Ride],
  ): Double = {
    val remaining = remainingFloors(route)

    val timeRides = elevatorRides.map {
      ride => {
        val stops = actualFloors(remaining, ride.end).size
        val pathRoute = path(route.floor :: actualFloors(remaining, ride.end))
        stops * stopTime + pathRoute * pathTime
      }
    }

    val timeAffected = floorAffectedRides.map {
      ride => {
        val stops = actualFloors(remaining, ride.start, ride.end).size
        val pathRoute = path(route.floor :: actualFloors(remaining, ride.start, ride.end))
        stops * stopTime + pathRoute * pathTime
      }
    }

    timeAffected.sum + timeRides.sum
  }

  def remainingFloors(route: Route): List[Double] = {
    def toFloors(opt: Option[Way]): List[Double] = opt.map(_.floors.toList.map(_.toDouble)).getOrElse(List.empty[Double])
    val restOfForward = route.forward.map(restOfWay(_, route.floor))
    toFloors(restOfForward) ++ toFloors(route.backward) ++ toFloors(route.tail)
  }

  def path(floors: List[Double]): Double = floors match {
    case Nil => 0.0
    case _ => {
      val pairs = floors.init.zip(floors.tail).toList
      pairs.foldLeft(0.0)((acc, pair) => acc + math.abs(pair._1 - pair._2))
    }
  }

  def actualFloors(floors: List[Double], destinationFloor: Int): List[Double] =
    floors.slice(0, floors.indexOf(destinationFloor) + 1)

  def actualFloors(floors: List[Double], pickupFloor: Int, destinationFloor: Int): List[Double] = {
    val pickupIndex = floors.indexWhere(_ eq pickupFloor)
    val destinationIndex = floors.indexWhere(_ eq destinationFloor, pickupIndex)
    floors.slice(0, pickupIndex + 1) ++ floors.slice(pickupIndex + 1, destinationIndex + 1)
  }
}
