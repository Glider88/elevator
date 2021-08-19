package actor.oop_style

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.ActorContext
//import scala.collection.immutable.SortedSet
import actor.domain._
import scala.concurrent.duration._
//import akka.actor.typed.scaladsl.TimerScheduler

object Elevator {
  sealed trait Command
  final case class CalculateDistanceDiff(newRide: Ride, affectedRides: Set[Ride], replyTo: ActorRef[System.DistanceDiff]) extends Command
  final case class AddRide(ride: Ride) extends Command
  final case object Tick extends Command

  def apply(ui: ActorRef[UI.Command], elevatorNumber: Int, system: ActorRef[System.Command]): Behavior[Elevator.Command] = {
    Behaviors.setup { context => 
      Behaviors.withTimers { timers =>
        timers.startTimerWithFixedDelay(Tick, 30.milliseconds)
        new Elevator(context, ui, system, elevatorNumber)
      }
    }
  }
}

class Elevator (
  context: ActorContext[Elevator.Command],
  ui: ActorRef[UI.Command],
  system: ActorRef[System.Command],
  elevatorNumber: Int
) extends AbstractBehavior[Elevator.Command](context) {
  import Elevator._

  var route = Route()
  var currentFloor = 10
  var stopTime = 0
  var afterStop = false

  override def onMessage(msg: Elevator.Command): Behavior[Elevator.Command] = {
    msg match {
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
        this

      case AddRide(ride) =>
        //context.log.info(s"receive Elevator.AddRide($ride)")
        val normalRide = Ride(ride.start * 10, ride.end * 10)
        val newRoute = route.addRide(currentFloor, normalRide)
        route = newRoute
        this

      case Tick => {
        //context.log.info(s"Elevator.Tick($currentFloor, $route, $stopTime, $afterStop)")
        ui ! UI.ElevatorCommand(elevatorNumber, currentFloor)

        if (stopTime > 0) {
          stopTime = stopTime - 1
        } else {
          route.forward match {
            case Some(forwardWay) => {

              val restWay = forwardWay.restOfWay(currentFloor)
              val newCurrentFloor = if (forwardWay.direction == Up) currentFloor + 1 else currentFloor - 1

              if (restWay.floors.head == currentFloor) {
                if (afterStop) {
                  if (forwardWay.restOfWay(newCurrentFloor).floors.isEmpty) {
                    route = Route(forward = route.backward, backward = route.tail, tail = Option.empty[Way])
                  } else {
                    currentFloor = newCurrentFloor
                  }

                  stopTime = 0
                  afterStop = false

                } else {
                  system ! System.ElevatorArrived(currentFloor / 10, context.self)

                  stopTime = 10
                  afterStop = true
                }
              } else {
                currentFloor = newCurrentFloor
                stopTime = 0
              }
            }
            case None => {
              stopTime = 0
            }
          }
        }

        this
      }
    }
  }
}
