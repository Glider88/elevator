package test.catz.domain

import org.scalatest.funspec.AnyFunSpec
import monocle.syntax.all._
import _root_.catz.domain.DomainCore._
import _root_.catz.domain.DomainElevator._
import test.SyntaxSugar

class DomainElevatorTest extends AnyFunSpec with SyntaxSugar {
  describe("Domain Elevator") {
    describe("Route") {
      it("add first ride") {
        assert(addRide(R(1.0), Ride(1, 2)) === R(1.0, U(1, 2)))
        assert(addRide(R(2.0), Ride(2, 1)) === R(2.0, D(2, 1)))

        assert(addRide(R(1.0), Ride(2, 3)) === R(1.0, U(2, 3)))
        assert(addRide(R(9.0), Ride(3, 2)) === R(9.0, D(3, 2)))

        assert(addRide(R(1.0), Ride(3, 2)) === R(1.0, U(3), D(2)))
        assert(addRide(R(9.0), Ride(1, 2)) === R(9.0, D(1), U(2)))
      }

      it("ride dont update route") {
        assert(addRide(R(1.0, U(2, 3)), Ride(2, 3)) === R(1.0, U(2, 3)))
      }

      it("add ride along the way") {
        assert(addRide(R(1.0, U(2, 3)), Ride(4, 5)) === R(1.0, U(2, 3, 4, 5)))
        assert(addRide(R(4.0, U(2, 3)), Ride(4, 5)) === R(4.0, U(2, 3, 4, 5)))
        assert(addRide(R(9.0, D(3, 2)), Ride(5, 4)) === R(9.0, D(5, 4, 3, 2)))
        assert(addRide(R(5.0, D(3, 2)), Ride(5, 4)) === R(5.0, D(5, 4, 3, 2)))
      }

      it("add ride with same direction but behind current floor") {
        assert(addRide(R(3.0, U(3, 4)), Ride(1, 2)) === R(3.0, U(3, 4), D(1), U(2)))
        assert(addRide(R(4.0, U(3, 4)), Ride(1, 2)) === R(4.0, U(3, 4), D(1), U(2)))
        assert(addRide(R(5.0, D(5, 4)), Ride(7, 6)) === R(5.0, D(5, 4), U(7), D(6)))
        assert(addRide(R(4.0, D(5, 4)), Ride(7, 6)) === R(4.0, D(5, 4), U(7), D(6)))
        assert(addRide(R(3.0, U(1, 2, 3)), Ride(1, 2)) === R(3.0, U(1, 2, 3), D(1), U(2)))
        assert(addRide(R(1.0, D(3, 2, 1)), Ride(3, 2)) === R(1.0, D(3, 2, 1), U(3), D(2)))
      }

      it("add ride with reverse direction") {
        assert(addRide(R(1.0, U(1, 2)), Ride(4, 3)) === R(1.0, U(1, 2, 4), D(3)))
        assert(addRide(R(4.0, D(4, 3)), Ride(1, 2)) === R(4.0, D(4, 3, 1), U(2)))
        assert(addRide(R(1.0, U(1, 2)), Ride(2, 1)) === R(1.0, U(1, 2), D(1)))
        assert(addRide(R(2.0, D(2, 1)), Ride(1, 2)) === R(2.0, D(2, 1), U(2)))
      }
    }

    describe("Moving elevator") {
      val move = elevatorMove(0.1, 10)(_)
      val empty = Set.empty[User]

      val idle = Elevator(1, R(1.0), 0, false, empty, Some(1))
      it("idle") {
        assert(move(idle) === Elevator(1, R(1.0), 10, true, empty, Some(1)))
      }

      val idleWithRouteUp = Elevator(1, R(1.0, U(2)), 0, true, empty, Some(1))
      val idleWithRouteDw = Elevator(1, R(2.0, D(1)), 0, true, empty, Some(1))
      it("start moving because accepting first ride") {
        assert(move(idleWithRouteUp) === Elevator(1, R(1.1, U(2)), 0, false, empty, None))
        assert(move(idleWithRouteDw) === Elevator(1, R(1.9, D(1)), 0, false, empty, None))
      }

      val movingUp = Elevator(1, R(1.5, U(2)), 0, false, empty, None)
      val movingDw = Elevator(1, R(1.5, D(1)), 0, false, empty, None)
      it("moving between floors") {
        assert(move(movingUp) === movingUp.focus(_.route.floor).replace(1.6))
        assert(move(movingDw) === movingDw.focus(_.route.floor).replace(1.4))
      }

      val arrivalUp = Elevator(1, R(1.9, U(2)), 0, false, empty, None)
      val arrivalDw = Elevator(1, R(1.1, D(1)), 0, false, empty, None)
      it("arrival") {
        assert(move(arrivalUp) === Elevator(1, R(2.0, U(2)), 10, true, empty, Some(2)))
        assert(move(arrivalDw) === Elevator(1, R(1.0, D(1)), 10, true, empty, Some(1)))
      }

      val stand = Elevator(1, R(2.0, U(2)), 10, true, empty, Some(2))
      it("stand at stop") {
        assert(move(stand) === stand.copy(stopTime = 9))
      }

      val finishStop = Elevator(1, R(2.0, U(2)), 1, true, empty, Some(2))
      it("finish stop") {
        assert(move(finishStop) === finishStop.copy(stopTime = 0))
      }

      val finishRouteUp = Elevator(1, R(2.0, U(2)), 0, true, empty, Some(2))
      val finishRouteDw = Elevator(1, R(2.0, D(2)), 0, true, empty, Some(2))
      it("finished route") {
        assert(move(finishRouteUp) === finishRouteUp.copy(route = R(2.0)))
        assert(move(finishRouteDw) === finishRouteDw.copy(route = R(2.0)))
      }

      val prevRoute = R(2.0, U(2), D(1), U(3))
      val newRoute = R(2.0, D(1), U(3))
      val changeRoute = Elevator(1, prevRoute, 0, true, empty, Some(2))
      it("turn around") {
        assert(move(changeRoute) === changeRoute.copy(route = newRoute))
      }

      val e0 = Elevator(1, R(1.1, U(2)), 0, false, empty, None)
      val e1 = move(e0)
      val e2 = move(e1)
      it("multiple moving") {
        assert(e2 === e0.focus(_.route.floor).replace(1.3))
      }
    }
  }
}