package actor.domain

object Route {
  def ahead(direction: Direction, base: Int, floor: Int): Boolean = if (direction == Up) floor >= base else floor <= base
  def apply(): Route = Route(Option.empty[Way], Option.empty[Way], Option.empty[Way])
}

final case class Route(forward: Option[Way], backward: Option[Way], tail: Option[Way]) {
  def addRide(currentFloor: Int, ride: Ride): Route = {
    forward match {
      case None => {
        if (ride.start == currentFloor) {
          this.copy(forward = Some(Way.make(ride.direction, ride.start, ride.end)))
        } else {
          val initDirection = if (ride.start > currentFloor) Up else Down
          if (initDirection == ride.direction) {
            this.copy(forward = Some(Way.make(initDirection, ride.start, ride.end)))
          } else {
            this.copy(forward = Some(Way.make(initDirection, ride.start)), backward = Some(Way.make(ride.direction, ride.end)))
          }
        }
      }
      case Some(forwardWay) => {
        def addOrCreate(wayOption: Option[Way], direction: Direction, floors: Int*): Way = {
          wayOption match {
            case Some(way) => way.addFloors(floors: _*)
            case None => Way.make(direction, floors: _*)
          }
        }

        if (forwardWay.direction != ride.direction) {
          // backward
          if (Route.ahead(forwardWay.direction, currentFloor, ride.start)) {
            this.copy(
              forward = Some(addOrCreate(forward, forwardWay.direction, ride.start)),
              backward = Some(addOrCreate(backward, ride.direction, ride.end))
            )
          } else {
            this.copy(
              backward = Some(addOrCreate(backward, ride.direction, ride.start, ride.end))
            )
          }
        } else if (Route.ahead(ride.direction, currentFloor, ride.start)) {
          // forward
          this.copy(
            forward = Some(addOrCreate(forward, forwardWay.direction, ride.start, ride.end))
          )
        } else {
          // tail
          if (Route.ahead(ride.direction.reversed, forwardWay.floors.last, ride.start)) {
            this.copy(
              backward = Some(addOrCreate(backward, ride.direction.reversed, ride.start)),
              tail = Some(addOrCreate(tail, ride.direction, ride.end))
            )
          } else {
            this.copy(
              tail = Some(addOrCreate(tail, ride.direction, ride.start, ride.end))
            )
          }
        }
      }
    }
  }

  def time(currentFloor: Int, finishFloor: Int): Int = {
    def toFloors(opt: Option[Way]): List[Int] = opt.map(_.floors.toList).getOrElse(List.empty[Int])
    
    val restOfForward = this.forward.map(_.restOfWay(currentFloor))

    val floorsWithoutPassing = toFloors(restOfForward) ++ toFloors(this.backward) ++ toFloors(this.tail)
    val userFloors = floorsWithoutPassing.slice(0, floorsWithoutPassing.indexOf(finishFloor) + 1)
    val floors = currentFloor :: userFloors

    val pairs = floors.init.zip(floors.tail).toList

    val path = pairs.foldLeft(0)((acc, pair) => acc + math.abs(pair._1 - pair._2))
    val stops = userFloors.size

    path + stops * 30
  }

  override def toString = {
    def print(w: Option[Way]) = 
      w match {
        case Some(w) => s"$w"
        case None => "_"
      }
    s"[" + print(forward) + "   " + print(backward) + "   " + print(tail) + "]"
  }
}
