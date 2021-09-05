package catz.domain

import DomainCore._
import DoubleComparison._
import monocle.syntax.all._

object DomainElevator {
  def addRide(route: Route, ride: Ride): Route = {
    route.forward match {
      case None => {
        if (ride.start == route.floor) {
          route.copy(forward = Some(Way.make(direction(ride), ride.start, ride.end)))
        } else {
          val initDirection = if (ride.start > route.floor) Up else Down
          if (initDirection == direction(ride)) {
            route.copy(forward = Some(Way.make(initDirection, ride.start, ride.end)))
          } else {
            route.copy(forward = Some(Way.make(initDirection, ride.start)), backward = Some(Way.make(direction(ride), ride.end)))
          }
        }
      }
      case Some(forwardWay) => {
        def addOrCreate(wayOption: Option[Way], direction: Direction, floors: Int*): Way = {
          def addFloors(way: Way, floors: Int*): Way = way.copy(floors = way.floors ++ floors)
          wayOption match {
            case Some(way) => addFloors(way, floors: _*)
            case None => Way.make(direction, floors: _*)
          }
        }

        if (forwardWay.direction != direction(ride)) {
          // backward
          if (ahead(forwardWay.direction, route.floor, ride.start)) {
            route.copy(
              forward = Some(addOrCreate(route.forward, forwardWay.direction, ride.start)),
              backward = Some(addOrCreate(route.backward, direction(ride), ride.end))
            )
          } else {
            route.copy(
              backward = Some(addOrCreate(route.backward, direction(ride), ride.start, ride.end))
            )
          }
        } else if (ahead(direction(ride), route.floor, ride.start)) {
          // forward
          route.copy(
            forward = Some(addOrCreate(route.forward, forwardWay.direction, ride.start, ride.end))
          )
        } else {
          // tail
          if (ahead(reversed(direction(ride)), forwardWay.floors.last.toDouble, ride.start)) {
            route.copy(
              backward = Some(addOrCreate(route.backward, reversed(direction(ride)), ride.start)),
              tail = Some(addOrCreate(route.tail, direction(ride), ride.end))
            )
          } else {
            route.copy(
              tail = Some(addOrCreate(route.tail, direction(ride), ride.start, ride.end))
            )
          }
        }
      }
    }
  }

  def elevatorMove(moveStep: Double, fullStopTime: Int)(elevator: Elevator): Elevator = {
    val route = elevator.route
    val stopTime = elevator.stopTime
    val atStop = elevator.atStop

    route.forward match {
      case None => elevator.copy(stopTime = fullStopTime, atStop = true)
      case Some(forwardWay) => {
        val restWay = restOfWay(forwardWay, route.floor)
        val newFloor = if (forwardWay.direction == Up)
          round(route.floor + moveStep)
          else round(route.floor - moveStep)

        if (atStop) {
          if (stopTime > 0) {
            elevator.focus(_.stopTime).modify(_ - 1)
          } else {
            if (restOfWay(forwardWay, newFloor).floors.isEmpty) {
              elevator.copy(route = Route(route.floor, forward = route.backward, backward = route.tail, tail = None))
            } else {
              elevator
                .focus(_.route.floor).replace(newFloor)
                .copy(atStop = false, arrivedFloor = None)
            }
          }
        } else {
          if (restWay.floors.head eq newFloor) {
            elevator
              .focus(_.route.floor).replace(newFloor)
              .copy(stopTime = fullStopTime, atStop = true, arrivedFloor = Some(newFloor.toInt))
          } else {
            elevator.focus(_.route.floor).replace(newFloor)
          }
        }
      }
    }
  }
}