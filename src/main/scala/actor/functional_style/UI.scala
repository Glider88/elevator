package actor.functional_style

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.ActorRef
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws._
import akka.stream.scaladsl._
import akka.http.scaladsl.server.Directives._
import scala.concurrent.duration._
import akka.stream.OverflowStrategy
import io.circe.generic.auto._
import io.circe.syntax._
import akka.stream.typed.scaladsl.ActorSource
import akka.stream.Materializer

object UI {
  sealed trait Command
  final case class Setup(numberToElevator: Map[Int, ActorRef[Elevator.Command]], numberToFloor: Map[Int, ActorRef[Floor.Command]]) extends Command
  final case class ElevatorCommand(id: Int, floor: Int) extends Command
  final case class UserCommand(id: String, from: Int, to: Int, status: String, elevator: Option[ActorRef[Elevator.Command]]) extends Command
  final case object Tick extends Command

  final case class StateRequest(elevators: Map[Int, ElevatorState], users: Map[String, UserState], `type`: String = "tick")
  final case class SetupRequest(elevatorNumber: Int, floorNumber: Int, `type`: String = "setup")

  final case class UserState(from: Int, to: Int, status: String, elevator: Option[Int])
  final case class ElevatorState(floor: Int)

  def apply(): Behavior[UI.Command] = {
    Behaviors.setup { context =>
      implicit val system = akka.actor.ActorSystem()
      implicit val materializer = Materializer(context)

      val (wsActor, wsSource) = ActorSource.actorRef[Message](
        PartialFunction.empty,
        PartialFunction.empty,
        bufferSize = 32,
        overflowStrategy = OverflowStrategy.dropHead
      ).preMaterialize()

      val flow = Flow.fromSinkAndSource(Sink.ignore, wsSource)
      val route = path("ws")(handleWebSocketMessages(flow))
      val _ = Http().newServerAt("localhost", 12346).bindFlow(route)

      Behaviors.withTimers { timers =>
        timers.startTimerWithFixedDelay(UI.Tick, 30.milliseconds)
        val ui = new UI(wsActor)
        ui.nextBehavior(Map.empty[Int, ElevatorState], Map.empty[String, UserState], Map.empty[ActorRef[Elevator.Command], Int])
      }
    }
  }
}

class UI private (wsActor: ActorRef[Message])
{
  import UI._

  private def nextBehavior(
    elevatorMap: Map[Int, ElevatorState],
    userMap: Map[String, UserState],
    elevatorToNumber: Map[ActorRef[Elevator.Command], Int]
  ): Behavior[UI.Command] = {
    Behaviors.receiveMessage {
      case Setup(numberToElevator, numberToFloor) =>
        //context.log.info(s"receive UI.Setup($numberToElevator, $numberToFloor)")
        wsActor ! TextMessage(SetupRequest(numberToElevator.size, numberToFloor.size).asJson.noSpaces)
        nextBehavior(elevatorMap, userMap, numberToElevator.map(_.swap))
      case ElevatorCommand(id, floor) =>
        //context.log.info(s"receive UI.ElevatorCommand($id, $floor)")
        nextBehavior(elevatorMap + (id -> ElevatorState(floor)), userMap, elevatorToNumber)
      case UserCommand(id, from, to, status, elevator) =>
        //context.log.info(s"receive UI.UserCommand($id, $from, $to, $status. $elevator)")
        val elevatorNumber = elevator match {
          case Some(e) => {
            elevatorToNumber.get(e) match {
              case Some(a) => {
                Some(a)
              }
              case None => {
                Option.empty[Int]
              }
            }
          }
          case None => {
            Option.empty[Int]
          }
        }
        nextBehavior(elevatorMap, userMap + (id -> UserState(from, to, status, elevatorNumber)), elevatorToNumber)
      case Tick =>
        //context.log.info(s"receive UI.Tick($elevatorMap, $userMap)")
        wsActor ! TextMessage(StateRequest(elevatorMap, userMap).asJson.noSpaces)
        nextBehavior(Map.empty[Int, ElevatorState], Map.empty[String, UserState], elevatorToNumber)
    }
  }
}
