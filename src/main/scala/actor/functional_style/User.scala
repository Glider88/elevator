package actor.functional_style

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
//import actor.domain.Route
import actor.domain.Ride
//import akka.NotUsed

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
    val user = new User(ui, name, startFloor, endFloor)
    user.nextBehavior(Option.empty[ActorRef[Elevator.Command]], Option.empty[ActorRef[Elevator.Command]])
  }
}

class User private (
  ui: ActorRef[UI.Command],
  name: String,
  startFloor: Int,
  endFloor: Int
) {
  import User._

  private def nextBehavior(matchedElevator: Option[ActorRef[Elevator.Command]], pickedElevator: Option[ActorRef[Elevator.Command]]): Behavior[User.Command] = {
    Behaviors.setup { context =>
      Behaviors.receiveMessage {
        case MatchedEvelator(elevator) =>
          //context.log.info(s"receive User.MatchedEvelator: $elevator")
          nextBehavior(Some(elevator), pickedElevator)
        case Boarding(elevator) =>
          //context.log.info(s"receive User.Boarding: $elevator")
          ui ! UI.UserCommand(name, startFloor, endFloor, "ride", Some(elevator))
          nextBehavior(matchedElevator, Some(elevator))
        case UnBoarding(elevator) =>
          //context.log.info(s"receive User.UnBoarding: $elevator")
          ui ! UI.UserCommand(name, startFloor, endFloor, "finished", Some(elevator))
          nextBehavior(Option.empty[ActorRef[Elevator.Command]], Option.empty[ActorRef[Elevator.Command]])
        case ElevatorArrived(floor, elevator) =>
          // context.log.info(s"receive User.ElevatorArrived(floor: $floor, elevator: $elevator) state: (start: $startFloor, end: $endFloor, match: $matchedElevator, pick: $pickedElevator)")
          if (floor == startFloor && elevator == matchedElevator.getOrElse(false)) {
            context.self ! Boarding(elevator)
          }

          if (floor == endFloor && elevator == pickedElevator.getOrElse(false)) {
            context.self ! UnBoarding(elevator)
          }

          Behaviors.same
        case RequestAffectedElevator(replyTo) => 
          // context.log.info(s"receive User.RequestAffectedElevator($replyTo)")
          val affected = if (pickedElevator.isDefined) pickedElevator else matchedElevator
          replyTo ! AffectedElevator(context.self, Ride(startFloor, endFloor), affected)
          
          Behaviors.same
      }
    }
  }
}
