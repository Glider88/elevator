package actor.functional_style

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.ActorRef
//import akka.actor.typed.scaladsl.ActorContext
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws._
import akka.stream.ActorMaterializer
import akka.stream.scaladsl._
import akka.http.scaladsl.server.Directives._
import scala.concurrent.duration._
// import scala.io.StdIn
import akka.stream.OverflowStrategy
//import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
//import akka.stream.SystemMaterializer
//import akka.actor.ClassicActorSystemProvider

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
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    //SystemMaterializer(implicitly[ClassicActorSystemProvider].classicSystem).materializer

    val (wsActor, wsSource) = Source.actorRef[Message](32, OverflowStrategy.dropHead).preMaterialize()
    val flow = Flow.fromSinkAndSource(Sink.ignore, wsSource)

    val route = path("ws")(handleWebSocketMessages(flow))
    val _ = Http().bindAndHandle(route, "localhost", 12346)

    Behaviors.setup { _ =>
      Behaviors.withTimers { timers =>
        timers.startTimerWithFixedDelay(UI.Tick, 30.milliseconds)
        val ui = new UI(wsActor)//, context)
        ui.nextBehavior(Map.empty[Int, ElevatorState], Map.empty[String, UserState], Map.empty[ActorRef[Elevator.Command], Int])
      }
    }
  }
}

class UI private (wsActor: akka.actor.ActorRef)//, context: ActorContext[UI.Command])
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