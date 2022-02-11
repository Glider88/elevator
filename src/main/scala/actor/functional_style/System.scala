package actor.functional_style

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.ActorContext
import akka.http.scaladsl.server.Directives._
import actor.domain.Ride

object System {
  sealed trait Command
  final case class ElevatorArrived(floor: Int, elevator: ActorRef[Elevator.Command]) extends Command
  final case class RegisterNewUser(user: ActorRef[User.Command]) extends Command
  final case class DeleteUser(user: ActorRef[User.Command]) extends Command
  final case class CallElevator(user: ActorRef[User.Command], ride: Ride, replyTo: ActorRef[CallButton.Command]) extends Command

  def apply(): Behavior[System.Command] = {
    Behaviors.setup { context =>
      val ui = context.spawn(UI(), "UI")
      val elevators = (1 to 3).map(n => n -> context.spawn(Elevator(ui, n, context.self), s"elevator_$n")).toMap
      val buttons = (1 to 9).map(n => n -> context.spawn(CallButton(n, context.self), s"call_button_$n")).toMap
      val floors = (1 to 9).map(n => n -> context.spawn(Floor(ui, n, buttons(n), context.self, 9), s"floor_$n")).toMap

      ui ! UI.Setup(elevators, floors)

      val system = new System(context)
      system.nextBehavior(Set.empty[ActorRef[User.Command]], elevators.values.toSet)
    }
  }

  def randomString(length: Int): String = {
    import scala.util.Random
    val alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    (1 to length).map(_ => alpha.apply(Random.nextInt(alpha.length))).mkString
  }

  sealed trait DiffCommand
  final case class ElevatorAndAffectedRides(elevator2rides: Map[ActorRef[Elevator.Command], Set[Ride]]) extends DiffCommand
  final case class DistanceDiff(newUserTime: Int, affectedUsersTimeDiff: Int, elevator: ActorRef[Elevator.Command]) extends DiffCommand

  def affectedUsersActor(
    users: Set[ActorRef[User.Command]],
    replyTo: ActorRef[System.ElevatorAndAffectedRides]
  ): Behavior[User.AffectedElevator] = {
    def nextBehavior(userProgress: Map[ActorRef[User.Command], Option[User.AffectedElevator]]): Behavior[User.AffectedElevator] = {
      Behaviors.setup { context =>
        Behaviors.receiveMessage {
          case a: User.AffectedElevator =>
            context.log.debug(s"Received System.affectedUsersActor.AffectedElevator($a)")
            val newUserProgress = userProgress + (a.user -> Some(a))
            if (newUserProgress.values.filter(_.isEmpty).isEmpty) {
              val affectedElevators = newUserProgress.values.flatten

              val elevator2rides = affectedElevators.foldLeft(Map.empty[ActorRef[Elevator.Command], Set[Ride]]) {
                (acc, affected) => {
                  affected.elevator match {
                    case Some(elevator) => acc.get(elevator) match {
                      case Some(set) => acc + (elevator -> (set + affected.ride))
                      case None => acc + (elevator -> Set(affected.ride))
                    }
                    case None => acc
                  }
                }
              }

              replyTo ! ElevatorAndAffectedRides(elevator2rides)
              Behaviors.stopped
            } else {
              nextBehavior(newUserProgress)
            }
          case _ =>
            Behaviors.unhandled
        }
      }
    }

    val userProgress = users.map(u => (u, Option.empty[User.AffectedElevator])).toMap
    nextBehavior(userProgress)
  }

  def elevatorDistanceDiffActor(
    user: ActorRef[User.Command],
    elevators: Set[ActorRef[Elevator.Command]],
    newRide: Ride,
    replyTo: ActorRef[CallButton.Command]
  ): Behavior[System.DiffCommand] = {  
    def nextBehavior(elevatorProgress: Map[ActorRef[Elevator.Command], Option[DistanceDiff]]): Behavior[System.DiffCommand] = {
      Behaviors.setup { context =>
        Behaviors.receiveMessage {
          case ElevatorAndAffectedRides(elevator2rides) =>
            context.log.debug(s"Received System.elevatorDistanceDiffActor.ElevatorAndAffectedRides($elevator2rides)")
            elevators.foreach(elevator => {
              elevator2rides.get(elevator) match {
                case Some(rides) => elevator ! Elevator.CalculateDistanceDiff(newRide, rides, context.self)
                case None => elevator ! Elevator.CalculateDistanceDiff(newRide, Set.empty[Ride], context.self)
              }
            })

            Behaviors.same
          case dd: DistanceDiff =>
            context.log.debug(s"Received System.elevatorDistanceDiffActor.$dd")
            val newElevatorProgress = elevatorProgress + (dd.elevator -> Some(dd))
            if (newElevatorProgress.values.filter(_.isEmpty).isEmpty) {
              val distanceDiffs = newElevatorProgress.values.flatten

              val elevator2usersDiff = distanceDiffs.map(dd => (dd.elevator -> dd.affectedUsersTimeDiff)).toMap
              val elevator2newUserTime = distanceDiffs.map(dd => (dd.elevator -> dd.newUserTime)).toMap

              val minUserTime = elevator2newUserTime.minBy(_._2)._2
              val elevator2newUserDiff = elevator2newUserTime.map {
                case (elevator, newUserDiff) => (elevator, newUserDiff - minUserTime)
              }

              val elevator2AllDiff = elevator2newUserDiff.map {
                case (elevator, diff) => (elevator, elevator2usersDiff(elevator) + diff)
              }

              val properElevator = elevator2AllDiff.minBy(_._2)._1
              replyTo ! CallButton.ProperElevator(user, properElevator, newRide)
              Behaviors.stopped
            } else {
              nextBehavior(newElevatorProgress)
            }
          case _ =>
            Behaviors.unhandled
        }
      }
    }

    val elevatorProgress = elevators.map(e => (e, Option.empty[DistanceDiff])).toMap
    nextBehavior(elevatorProgress)
  }
}

class System private (context: ActorContext[System.Command])
{
  import System._

  private def nextBehavior(users: Set[ActorRef[User.Command]], elevators: Set[ActorRef[Elevator.Command]]): Behavior[System.Command] = {
    Behaviors.receiveMessage {
      case ElevatorArrived(floor, elevator) =>
        context.log.debug(s"Received System.ElevatorArrived($floor, $elevator)")
        users.foreach(_ ! User.ElevatorArrived(floor, elevator))
        Behaviors.same
      case RegisterNewUser(user) =>
        context.log.debug(s"Received System.RegisterNewUser: $user")
        nextBehavior(users + user, elevators)
      case DeleteUser(user) =>
        context.log.debug(s"Received System.DeleteUser: $user")
        nextBehavior( users - user, elevators)
      case CallElevator(user, newRide, replyTo) =>
        context.log.debug(s"Received System.CallElevator($user, $newRide, $replyTo)")
        val name = randomString(10)
        val elevatorDistanceDiff = context.spawn(elevatorDistanceDiffActor(user, elevators, newRide, replyTo), s"elevatorDistanceDiffActor-$name")
        val affectedUsers = context.spawn(affectedUsersActor(users, elevatorDistanceDiff), s"affectedUsersActor-$name")
        users.foreach(_ ! User.RequestAffectedElevator(affectedUsers))

        Behaviors.same
    }
  }
}