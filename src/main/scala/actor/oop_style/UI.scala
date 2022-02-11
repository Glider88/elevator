package actor.oop_style

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.AbstractBehavior
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
        new UI(context, wsActor)
      }
    }
  }
}

class UI(
  context: ActorContext[UI.Command],
  wsActor: ActorRef[Message]
) extends AbstractBehavior[UI.Command](context) {
  import UI._

  var elevatorMap = Map.empty[Int, ElevatorState]
  var userMap = Map.empty[String, UserState]
  var elevatorToNumber = Map.empty[ActorRef[Elevator.Command], Int]

  override def onMessage(msg: UI.Command): Behavior[UI.Command] = {
    msg match {
      case Setup(numberToElevator, numberToFloor) =>
        context.log.debug(s"Received UI.Setup($numberToElevator, $numberToFloor)")
        wsActor ! TextMessage(SetupRequest(numberToElevator.size, numberToFloor.size).asJson.noSpaces)
        elevatorToNumber = numberToElevator.map(_.swap)
        this
      case ElevatorCommand(id, floor) =>
        context.log.debug(s"Received UI.ElevatorCommand($id, $floor)")
        elevatorMap = elevatorMap + (id -> ElevatorState(floor))
        this
      case UserCommand(id, from, to, status, elevator) =>
        context.log.debug(s"Received UI.UserCommand($id, $from, $to, $status. $elevator)")
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
        context.log.debug(s"Received UI.Tick($elevatorMap, $userMap)")
        wsActor ! TextMessage(StateRequest(elevatorMap, userMap).asJson.noSpaces)
        userMap = Map.empty[String, UserState]
        elevatorMap = Map.empty[Int, ElevatorState]
        this
    }
  }
}
