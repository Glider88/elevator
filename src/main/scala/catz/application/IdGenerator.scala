package catz.application

import cats.effect.kernel.{Ref, Sync}
import cats.syntax.functor._

trait IdGenerator[F[_]] {
  def nextId: F[Int]
}

object LiveIdGenerator {
  def make[F[_]: Sync]: F[IdGenerator[F]] =
    Ref.of[F, Int](1).map(new LiveIdGenerator[F](_))
}

class LiveIdGenerator[F[_]] private (ref: Ref[F, Int]) extends IdGenerator[F] {
  def nextId: F[Int] = ref.getAndUpdate(_ + 1)
}
