package cats

import scala.util.Random
import cats.effect.IO


object LiveRandomIntExclude {
  def nextInt(from: Int, to: Int, exclude: Int): IO[Int] = IO(randomIntExclude(from, to, exclude))

  private def randomIntExclude(from: Int, to: Int, exclude: Int): Int = {
    val random = Random.between(from, to)
    if (random == exclude) {
      randomIntExclude(from, to, exclude)
    } else {
      random
    }
  }
}
