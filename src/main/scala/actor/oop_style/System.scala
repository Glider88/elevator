package actor.oop_style

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.AbstractBehavior
//import akka.actor.ActorSystem
//import akka.http.scaladsl.Http
//import akka.http.scaladsl.model.ws._
//import akka.stream.ActorMaterializer
//import akka.stream.scaladsl._
import akka.http.scaladsl.server.Directives._
//import scala.concurrent.duration._
//import scala.io.StdIn
//import akka.stream.OverflowStrategy
//import actor.domain.Route
import actor.domain.Ride
//import akka.NotUsed

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

      new System(context, elevators.values.toSet)
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

  private class AffectedUsersActor(
    context: ActorContext[User.AffectedElevator],
    users: Set[ActorRef[User.Command]],
    replyTo: ActorRef[System.ElevatorAndAffectedRides]
  ) extends AbstractBehavior[User.AffectedElevator](context) {

    var userProgress: Map[ActorRef[User.Command], Option[User.AffectedElevator]] = users.map(u => (u, Option.empty[User.AffectedElevator])).toMap

    override def onMessage(msg: User.AffectedElevator): Behavior[User.AffectedElevator] = {
      msg match {
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
            userProgress = newUserProgress
          }
          this
        case _ =>
          Behaviors.unhandled
      }
    }
  }

  private class ElevatorDistanceDiffActor(
    context: ActorContext[System.DiffCommand],
    user: ActorRef[User.Command],
    elevators: Set[ActorRef[Elevator.Command]],
    newRide: Ride,
    replyTo: ActorRef[CallButton.Command]
  ) extends AbstractBehavior[System.DiffCommand](context) {

    var elevatorProgress = elevators.map(e => (e, Option.empty[DistanceDiff])).toMap

    override def onMessage(msg: System.DiffCommand): Behavior[System.DiffCommand] = {
      msg match {
        case ElevatorAndAffectedRides(elevator2rides) =>
          context.log.debug(s"Received System.elevatorDistanceDiffActor.ElevatorAndAffectedRides($elevator2rides)")
          elevators.foreach(elevator => {
            elevator2rides.get(elevator) match {
              case Some(rides) => elevator ! Elevator.CalculateDistanceDiff(newRide, rides, context.self)
              case None => elevator ! Elevator.CalculateDistanceDiff(newRide, Set.empty[Ride], context.self)
            }
          })

          this
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
            elevatorProgress = newElevatorProgress
          }

          this
        case _ =>
          Behaviors.unhandled
      }
    }
  }
}

class System (
  context: ActorContext[System.Command],
  elevators: Set[ActorRef[Elevator.Command]]
) extends AbstractBehavior[System.Command](context) {
  import System._

  var users = Set.empty[ActorRef[User.Command]]

  override def onMessage(msg: System.Command): Behavior[System.Command] = {
    msg match {
      case ElevatorArrived(floor, elevator) =>
        context.log.debug(s"Received System.ElevatorArrived($floor, $elevator)")
        users.foreach(_ ! User.ElevatorArrived(floor, elevator))
        this
      case RegisterNewUser(user) =>
        context.log.debug(s"Received System.RegisterNewUser: $user")
        users = users + user
        this
      case DeleteUser(user) =>
        context.log.debug(s"Received System.DeleteUser: $user")
        users = users - user
        this
      case CallElevator(user, newRide, replyTo) =>
        context.log.debug(s"Received System.CallElevator($user, $newRide, $replyTo)")
        val name = randomString(10)
        val elevatorDistanceDiff = context.spawn(
          Behaviors.setup(context => new ElevatorDistanceDiffActor(context, user, elevators, newRide, replyTo)),
          s"elevatorDistanceDiffActor-$name"
        )
        val affectedUsers = context.spawn(
          Behaviors.setup(context => new AffectedUsersActor(context, users, elevatorDistanceDiff)),
          s"affectedUsersActor-$name"
        )
        users.foreach(_ ! User.RequestAffectedElevator(affectedUsers))
        this
    }
  }
}
