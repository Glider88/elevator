package catz.domain

import cats._
import cats.syntax.all._
import DomainCore._
import catz.application.AppMonoliteState.AppState
import java.lang.System.lineSeparator

object Shows {
  val EOL = lineSeparator()

  implicit val showRide: Show[Ride] = Show.show(r => s"${direction(r)}(${r.start}, ${r.end})")

  implicit val showWay: Show[Way] = Show.show(w => s"${w.direction}: " + w.floors.mkString(", "))

  implicit val showRoute: Show[Route] = Show.show(r => {
    def print(ow: Option[Way]) = 
      ow match {
        case Some(w) => show"$w"
        case None => "_"
      }
    s"${r.floor} [" + print(r.forward) + "   " + print(r.backward) + "   " + print(r.tail) + "]"
  })

  implicit val showUser: Show[User] =
    Show.show(u => s"U${u.id}(${u.ride.start}-${u.ride.end} [${u.matchedElevatorNumber.getOrElse("")}])")

  implicit val showElevator: Show[Elevator] =
    Show.show(e => s"E${e.number}(${e.route.show} ${e.users.map(u => u.show).mkString(" ")} ${e.stopTime} ${e.atStop} ${e.arrivedFloor.getOrElse("_")})")

  implicit val showFloor: Show[Floor] =
    Show.show(f => s"F${f.number}(${f.waiting.map(u => u.show).mkString(" ")})(${f.finished.map(u => u.show).mkString(" ")})")

  implicit val showAppState: Show[AppState] =
    Show.show(s => {
      def list[A](m: Map[Int, A]): List[A] = m.toList.sortBy(_._1).map(i2v => i2v._2)
      EOL + list(s.elevators).map(_.show).mkString(EOL) + EOL +
      EOL +
      list(s.floors).reverse.map(_.show).mkString(EOL) + EOL +
      "-----------------------------"
    })
}
