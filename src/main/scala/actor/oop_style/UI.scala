package actor.oop_style

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws._
import akka.stream.ActorMaterializer
import akka.stream.scaladsl._
import akka.http.scaladsl.server.Directives._
import scala.concurrent.duration._
//import scala.io.StdIn
import akka.stream.OverflowStrategy
//import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

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

    val (wsActor, wsSource) = Source.actorRef[Message](32, OverflowStrategy.dropHead).preMaterialize()
    val flow = Flow.fromSinkAndSource(Sink.ignore, wsSource)

    val route = path("ws")(handleWebSocketMessages(flow))
    val _ = Http().bindAndHandle(route, "localhost", 12346)

    Behaviors.setup { context =>
      Behaviors.withTimers { timers =>
        timers.startTimerWithFixedDelay(UI.Tick, 30.milliseconds)
        new UI(context, wsActor)
      }
    }
  }
}

class UI(
  context: ActorContext[UI.Command],
  wsActor: akka.actor.ActorRef
) extends AbstractBehavior[UI.Command](context) {
  import UI._
    
  var elevatorMap = Map.empty[Int, ElevatorState]
  var userMap = Map.empty[String, UserState]
  var elevatorToNumber = Map.empty[ActorRef[Elevator.Command], Int]

  override def onMessage(msg: UI.Command): Behavior[UI.Command] = {
    msg match {
      case Setup(numberToElevator, numberToFloor) =>
        //context.log.info(s"receive UI.Setup($numberToElevator, $numberToFloor)")
        wsActor ! TextMessage(SetupRequest(numberToElevator.size, numberToFloor.size).asJson.noSpaces)
        elevatorToNumber = numberToElevator.map(_.swap)
        this
      case ElevatorCommand(id, floor) =>
        //context.log.info(s"receive UI.ElevatorCommand($id, $floor)")
        elevatorMap = elevatorMap + (id -> ElevatorState(floor))
        this
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
        userMap = userMap + (id -> UserState(from, to, status, elevatorNumber))
        this
      case Tick =>
        //context.log.info(s"receive UI.Tick($elevatorMap, $userMap)")
        wsActor ! TextMessage(StateRequest(elevatorMap, userMap).asJson.noSpaces)
        userMap = Map.empty[String, UserState]
        elevatorMap = Map.empty[Int, ElevatorState]
        this
    }
  }
}
