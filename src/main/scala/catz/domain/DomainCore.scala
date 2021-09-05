package catz.domain

import scala.collection.immutable.SortedSet
import DoubleComparison._

object DomainCore {
  sealed trait Direction
  final case object Up extends Direction
  final case object Down extends Direction

  def reversed(direction: Direction): Direction = if (direction == Up) Down else Up

  final case class User(id: Int, ride: Ride, matchedElevatorNumber: Option[Int])

  final case class Elevator(number: Int, route: Route, stopTime: Int, atStop: Boolean, users: Set[User], arrivedFloor: Option[Int])
  object Elevator {
    def makeStart(number: Int, floor: Double): Elevator =
      Elevator(number, Route(floor), 10, true, Set.empty[User], Some(floor.toInt))
  }
  
  final case class Floor(number: Int, waiting: Set[User], finished: Set[User])

  final case class Ride(start: Int, end: Int)

  def floors(ride: Ride): List[Int] = List(ride.start, ride.end)
  def direction(ride: Ride): Direction = if (ride.start > ride.end) Down else Up

  final case class Way(direction: Direction, floors: SortedSet[Int])
  object Way {
    def make(direction: Direction, floors: Int*): Way =
      if (direction == Up) makeUp(floors: _*) else makeDown(floors: _*)
    def makeUp(floors: Int*): Way = Way(Up, SortedSet(floors: _*))
    def makeDown(floors: Int*): Way = Way(Down, SortedSet(floors: _*)(Ordering.Int.reverse))
  }

  def restOfWay(way: Way, floor: Double): Way =
    way.copy(floors = way.floors.filter(f => ahead(way.direction, floor, f)))

  def ahead(direction: Direction, base: Double, floor: Int): Boolean =
    if (direction == Up) base lte floor else base gte floor

  final case class Route(floor: Double, forward: Option[Way], backward: Option[Way], tail: Option[Way])
  object Route {
    def apply(floor: Double): Route = Route(floor, None, None, None)
  }
}