package catz.application

import scala.util.Random
import cats.effect.IO

object LiveRandomInt {
  def nextInt(from: Int, to: Int, exclude: Int): IO[Int] = IO(randomIntExclude(from, to, exclude))
  def nextInt(from: Int, to: Int): IO[Int] = IO(Random.between(from, to))

  private def randomIntExclude(from: Int, to: Int, exclude: Int): Int = {
    val random = Random.between(from, to)
    if (random == exclude) {
      randomIntExclude(from, to, exclude)
    } else {
      random
    }
  }
}
