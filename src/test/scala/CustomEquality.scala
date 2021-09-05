package test

import scala.collection.immutable.SortedSet
import org.scalactic._

trait CustomEquality {
  implicit val sortedSetIntEq =
    new Equality[SortedSet[Int]] {
      def areEqual(a: SortedSet[Int], b: Any): Boolean =
        b match {
          case p: SortedSet[_] => a == p && a.ordering == p.ordering
          case _ => false
        }
    }
}

object CustomEquality extends CustomEquality
