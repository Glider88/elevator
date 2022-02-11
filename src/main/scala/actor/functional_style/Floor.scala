package actor.functional_style

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import scala.util.Random
import scala.concurrent.duration._

object Floor {
  sealed trait Command
  final case object GenerateNewUser extends Command

  def apply(ui: ActorRef[UI.Command], currentFloor: Int, callButton: ActorRef[CallButton.Command], system: ActorRef[System.Command], maxFloors: Int): Behavior[Floor.Command] = {
    Behaviors.setup { context =>
      Behaviors.withTimers { timers =>
        timers.startSingleTimer(GenerateNewUser, Random.between(3, 30).seconds)
        Behaviors.receiveMessage {
          case GenerateNewUser =>
            context.log.debug(s"Received Floor.GenerateNewUser")
            def randomIntExclude(from: Int, to: Int, exclude: Int): Int = {
              val random = Random.between(from, to)
              if (random == exclude) {
                randomIntExclude(from, to, exclude)
              } else {
                random
              }
            }

            val finishFloor = randomIntExclude(1, maxFloors + 1, currentFloor)
            val name = System.randomString(5)
            val user = context.spawn(User(ui, name, currentFloor, finishFloor), s"user_$name")

            system ! System.RegisterNewUser(user)
            callButton ! CallButton.CallElevator(user, finishFloor)

            timers.startSingleTimer(GenerateNewUser, Random.between(1, 30).seconds)
            Behaviors.same
        }
      }
    }
  }
}
