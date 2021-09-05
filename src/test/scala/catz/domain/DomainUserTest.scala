package test.catz.domain

import org.scalatest.funspec.AnyFunSpec
import scala.math.abs
import _root_.catz.domain.DomainCore._
import _root_.catz.domain.DomainUser._
import test.SyntaxSugar

class DomainUserTest extends AnyFunSpec with SyntaxSugar {
  describe("Domain User") {
    describe("Matching elevator") {
      describe("Calculate route time") {
        it("remaining floors") {
          assert(remainingFloors(R(1.0)) === List.empty[Double])
          assert(remainingFloors(R(3.0, U(1, 2, 3, 4), D(1), U(2))) === List(3.0, 4.0, 1.0, 2.0))
          assert(remainingFloors(R(3.1, U(1, 2, 3, 4), D(1), U(2))) === List(4.0, 1.0, 2.0))
        }

        it("path size") {
          assert(path(List.empty[Double]) === 0.0)
          assert(path(List(1.0)) === 0.0)
          assert(path(List(1.0, 3.0, 2.0, 4.0)) === 5.0)
          assert(path(List(5.1, 2.0, 4.0, 1.0)) === 8.1)
        }

        it("actual floors for elevator users") {
          assert(actualFloors(List.empty[Double], 1) === List.empty[Double])
          assert(actualFloors(List(1.0, 2.0), 3) === List.empty[Double])
          assert(actualFloors(List(1.0, 2.0, 3.0), 2) === List(1.0, 2.0))
        }

        it("actual floors for waiting users") {
          assert(actualFloors(List.empty[Double], 1, 2) === List.empty[Double])
          assert(actualFloors(List(1.0, 2.0), 3, 4) === List.empty[Double])
          assert(actualFloors(List(1.0, 5.0), 2, 3) === List.empty[Double])
          assert(actualFloors(List(1.0, 2.0, 3.0, 4.0), 2, 3) === List(1.0, 2.0, 3.0))
        }

        it("route time") {
          val time = routeTime(3, 1)(_, _, _)
          assert(time(R(1.0), Set.empty[Ride], Set.empty[Ride]) === 0.0)
          assert(time(R(1.0, U(7), D(2), U(4)), Set.empty[Ride], Set.empty[Ride]) === 0.0)
          assert(time(R(1.0), Set(Ride(1, 2)), Set(Ride(4, 3))) === 0.0)

          val elevator = abs(1.1 - 3) + 1 * 3
          val await = (abs(1.1 - 3) + abs(3 - 4) + (4 - 3)) + 3 * 3
          assert(time(R(1.1, U(3, 4), D(3), U(1)), Set(Ride(1, 3)), Set(Ride(4, 3))) === elevator + await)
        }
      }

      describe("Matching") {
        it("not affected rides awaiting users") {
          val empty = Set.empty[User]
          
          val u1 = User(1, Ride(1, 2), None)
          val u2 = User(2, Ride(1, 2), Some(2))
          val u3 = User(3, Ride(1, 2), Some(1))
          
          val f1 = Floor(1, Set(u1), empty)
          val f2 = Floor(2, Set(u2), empty)
          val f3 = Floor(3, empty, Set(u3))

          assert(affectedRides(1, List(f1)) == Set.empty[Ride])
          assert(affectedRides(1, List(f2)) == Set.empty[Ride])
          assert(affectedRides(1, List(f3)) == Set.empty[Ride])
        }

        it("affected rides awaiting users") {
          val empty = Set.empty[User]
          val u1 = User(1, Ride(1, 2), Some(1))
          val u2 = User(2, Ride(2, 1), Some(1))

          val f1 = Floor(1, Set(u1), empty)
          val f2 = Floor(2, Set(u2), empty)

          assert(affectedRides(1, List(f1, f2)) == Set(Ride(1, 2), Ride(2, 1)))
        }

        it("elevator matching") {
          val empty = Set.empty[User]

          val u1 = User(1, Ride(3, 1), Some(1))
          val u2 = User(2, Ride(2, 1), None)

          val f1 = Floor(1, empty, empty)
          val f2 = Floor(2, Set(u2), empty)
          val f3 = Floor(3, empty, empty)

          val e1 = Elevator(1, R(3.0), 0, true, Set(u1), Some(3))
          val e2 = Elevator(2, R(1.0), 0, true, empty, Some(1))

          val matchE = matchElevator(3, 1) _

          assert(matchE(List(f1, f2, f3), List(e1, e2), Ride(2, 1)) == e2)
        }
      }
    }
  }
}
