package actor.functional_style

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
//import scala.collection.immutable.SortedSet
//import akka.actor.typed.scaladsl.ActorContext
import actor.domain._
import scala.concurrent.duration._

object Elevator {
  sealed trait Command
  final case class CalculateDistanceDiff(newRide: Ride, affectedRides: Set[Ride], replyTo: ActorRef[System.DistanceDiff]) extends Command
  final case class AddRide(ride: Ride) extends Command
  final case object Tick extends Command

  def apply(ui: ActorRef[UI.Command], elevatorNumber: Int, system: ActorRef[System.Command]): Behavior[Elevator.Command] = {
    Behaviors.setup { _ =>
      Behaviors.withTimers { timers =>
        timers.startTimerWithFixedDelay(Tick, 30.milliseconds)
        val elevator = new Elevator(/*context, */ui, system, elevatorNumber)
        elevator.nextBehavior(Route(), 10, 0, false)
      }
    }
  }
}

class Elevator private (
  //context: ActorContext[Elevator.Command],
  ui: ActorRef[UI.Command],
  system: ActorRef[System.Command],
  elevatorNumber: Int
) {
  import Elevator._

  private def nextBehavior(
    route: Route,
    currentFloor: Int,
    stopTime: Int,
    afterStop: Boolean
  ): Behavior[Elevator.Command] = {
    Behaviors.receive { (context, message) =>
      message match {
        case CalculateDistanceDiff(newRide, affectedRides, replyTo) =>
          //context.log.info(s"receive Elevator.CalculateDistanceDiff($newRide, $affectedRides, $replyTo)")
          val normalNewRide = Ride(newRide.start * 10, newRide.end * 10)
          val normalEffectedRides = affectedRides.map(r => Ride(r.start * 10, r.end * 10))

          val potentialNewRoute = route.addRide(currentFloor, normalNewRide)

          val routeTime = normalEffectedRides.foldLeft(0) ((allTime, ride) => allTime + route.time(currentFloor, ride.end))
          val potentialRouteTime = normalEffectedRides.foldLeft(0) ((allTime, ride) => allTime + potentialNewRoute.time(currentFloor, ride.end))
          val affectedUsersTimeDiff = potentialRouteTime - routeTime

          val newUserTime = potentialNewRoute.time(currentFloor, normalNewRide.end)

          replyTo ! System.DistanceDiff(newUserTime, affectedUsersTimeDiff, context.self)
          Behaviors.same

        case AddRide(ride) =>
          //context.log.info(s"receive Elevator.AddRide($ride)")
          val normalRide = Ride(ride.start * 10, ride.end * 10)
          val newRoute = route.addRide(currentFloor, normalRide)
          nextBehavior(newRoute, currentFloor, stopTime, afterStop)

        case Tick => {
          ui ! UI.ElevatorCommand(elevatorNumber, currentFloor)

          if (stopTime > 0) {
            nextBehavior(route, currentFloor, stopTime - 1, afterStop)
          } else {
            route.forward match {
              case Some(forwardWay) => {
                
                val restWay = forwardWay.restOfWay(currentFloor)
                val newCurrentFloor = if (forwardWay.direction == Up) currentFloor + 1 else currentFloor - 1

                if (restWay.floors.head == currentFloor) {
                  if (afterStop) {
                    if (forwardWay.restOfWay(newCurrentFloor).floors.isEmpty) {
                      nextBehavior(Route(forward = route.backward, backward = route.tail, tail = Option.empty[Way]), currentFloor, 0, false)
                    } else {
                      nextBehavior(route, newCurrentFloor, 0, false)
                    }

                  } else {
                    system ! System.ElevatorArrived(currentFloor / 10, context.self)
                    nextBehavior(route, currentFloor, 10, true)
                  }
                } else {
                  nextBehavior(route, newCurrentFloor, 0, afterStop)
                }
              }
              case None => {
                nextBehavior(route, currentFloor, 0, afterStop)
              }
            }
          }
        }
      }
    }
  }
}
