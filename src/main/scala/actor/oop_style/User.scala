package actor.oop_style

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AbstractBehavior
import actor.domain.Ride
import akka.actor.typed.scaladsl.ActorContext

object User {
  sealed trait Command
  final case class MatchedEvelator(elevator: ActorRef[Elevator.Command]) extends Command
  final case class ElevatorArrived(floor: Int, elevator: ActorRef[Elevator.Command]) extends Command
  final case class Boarding(elevator: ActorRef[Elevator.Command]) extends Command
  final case class UnBoarding(elevator: ActorRef[Elevator.Command]) extends Command
  final case class RequestAffectedElevator(replyTo: ActorRef[User.AffectedElevator]) extends Command

  final case class AffectedElevator(user: ActorRef[User.Command], ride: Ride, elevator: Option[ActorRef[Elevator.Command]])

  def apply(ui: ActorRef[UI.Command], name: String, startFloor: Int, endFloor: Int): Behavior[User.Command] = {
    ui ! UI.UserCommand(name, startFloor, endFloor, "waiting", Option.empty[ActorRef[Elevator.Command]])
    Behaviors.setup(context => new User(context, ui, name, startFloor, endFloor))
  }
}

class User(
  context: ActorContext[User.Command],
  ui: ActorRef[UI.Command],
  name: String,
  startFloor: Int,
  endFloor: Int
) extends AbstractBehavior[User.Command](context) {
  import User._

  var matchedElevator = Option.empty[ActorRef[Elevator.Command]]
  var pickedElevator = Option.empty[ActorRef[Elevator.Command]]

  override def onMessage(msg: User.Command): Behavior[User.Command] = {
    msg match {
      case MatchedEvelator(elevator) =>
        context.log.debug(s"Received User.MatchedEvelator: $elevator")
        matchedElevator = Some(elevator)
        this
      case Boarding(elevator) =>
        context.log.debug(s"Received User.Boarding: $elevator")
        ui ! UI.UserCommand(name, startFloor, endFloor, "ride", Some(elevator))
        pickedElevator = Some(elevator)
        this
      case UnBoarding(elevator) =>
        context.log.debug(s"Received User.UnBoarding: $elevator")
        ui ! UI.UserCommand(name, startFloor, endFloor, "finished", Some(elevator))
        matchedElevator = Option.empty[ActorRef[Elevator.Command]]
        pickedElevator = Option.empty[ActorRef[Elevator.Command]]
        this
      case ElevatorArrived(floor, elevator) =>
        context.log.debug(s"Received User.ElevatorArrived(floor: $floor, elevator: $elevator) state: (start: $startFloor, end: $endFloor, match: $matchedElevator, pick: $pickedElevator)")
        if (floor == startFloor && elevator == matchedElevator.getOrElse(false)) {
          context.self ! Boarding(elevator)
        }

        if (floor == endFloor && elevator == pickedElevator.getOrElse(false)) {
          context.self ! UnBoarding(elevator)
        }

        this
      case RequestAffectedElevator(replyTo) => 
        context.log.debug(s"Received User.RequestAffectedElevator($replyTo)")
        val affected = if (pickedElevator.isDefined) pickedElevator else matchedElevator
        replyTo ! AffectedElevator(context.self, Ride(startFloor, endFloor), affected)
        
        this
    }
  }
}
