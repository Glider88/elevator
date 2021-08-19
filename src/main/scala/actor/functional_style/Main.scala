package actor.functional_style

import akka.actor.typed.ActorSystem
import actor.functional_style.System

object Main extends App {
  val system = ActorSystem(System(), "system")
}
