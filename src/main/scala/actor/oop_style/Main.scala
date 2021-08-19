package actor.oop_style

import akka.actor.typed.ActorSystem
import actor.oop_style.System

object Main extends App {
  val system = ActorSystem(System(), "system")
}
