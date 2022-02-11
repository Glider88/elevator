package actor.functional_style

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import actor.domain._

object CallButton {
  sealed trait Command
  final case class CallElevator(user: ActorRef[User.Command], requiredFloor: Int) extends Command
  final case class ProperElevator(user: ActorRef[User.Command], elevator: ActorRef[Elevator.Command], ride: Ride) extends Command

  def apply(currentFloor: Int, system: ActorRef[System.Command]): Behavior[CallButton.Command] = {
    Behaviors.setup { context =>
      Behaviors.receiveMessage {
        case CallElevator(user, requiredFloor) =>
          context.log.debug(s"Received CallButton.CallElevator($user, $requiredFloor)")
          system ! System.CallElevator(user, Ride(currentFloor, requiredFloor), context.self)
          Behaviors.same
        case ProperElevator(user, elevator, ride) =>
          context.log.debug(s"Received CallButton.ProperElevator($user, $elevator, $ride)")
          user ! User.MatchedEvelator(elevator)
          elevator ! Elevator.AddRide(ride)
          Behaviors.same
      }
    }
  }
}
