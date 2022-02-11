package actor.domain

import scala.collection.immutable.SortedSet

object Way {
  def make(direction: Direction, floors: Int*): Way =
    if (direction == Up) makeUp(floors: _*) else makeDown(floors: _*)

  def makeUp(floors: Int*): Way =
    Way(Up, SortedSet(floors: _*))

  def makeDown(floors: Int*): Way =
    Way(Down, SortedSet(floors: _*)(Ordering.Int.reverse))
}

final case class Way(direction: Direction, floors: SortedSet[Int]) {
  def addFloors(floors: Int*): Way =
    this.copy(floors = this.floors ++ floors)

  def restOfWay(currentFloor: Int): Way =
    this.copy(floors = floors.filter(floor => Route.ahead(direction, currentFloor, floor)))

  override def toString = s"$direction: " + floors.mkString(", ")
}
