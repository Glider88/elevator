package test.catz.domain

import org.scalatest.funspec.AnyFunSpec
import scala.collection.immutable.SortedSet
import _root_.catz.domain.DomainCore._
import test.CustomEquality

class DomainCoreTest extends AnyFunSpec with CustomEquality {
  describe("Domain Core") {
    describe("Direction") {
      it("reversed Up should be Down") {
        assert(reversed(Up) === Down)
      }

      it("reversed Down should be Up") {
        assert(reversed(Down) === Up)
      }

      it("ahead") {
        assert(ahead(Up, 1, 2) == true)
        assert(ahead(Up, 2, 1) == false)
        assert(ahead(Down, 1, 2) == false)
        assert(ahead(Down, 2, 1) == true)
      }
    }

    describe("Ride") {
      it("has floors") {
        assert(floors(Ride(2, 7)) === List(2, 7))
        assert(floors(Ride(4, 1)) === List(4, 1))
      }

      it("has direction") {
        assert(direction(Ride(1, 2)) === Up)
        assert(direction(Ride(2, 1)) === Down)
      }
    }

    describe("Way") {
      it("making Way") {
        assert(Way.make(Up, 3, 1, 2) === Way(Up, SortedSet(1, 2, 3)))
        assert(Way.make(Up, 3, 1, 2) !== Way(Down, SortedSet(1, 2, 3)))

        assert(Way.make(Down, 3, 1, 2) === Way(Down, SortedSet(1, 2, 3)))
        assert(Way.make(Down, 3, 1, 2) !== Way(Up, SortedSet(1, 2, 3)))

        assert(Way.makeUp(3, 1, 2) === Way(Up, SortedSet(1, 2, 3)))
        assert(Way.makeUp(3, 1, 2) !== Way(Down, SortedSet(1, 2, 3)))

        assert(Way.makeDown(3, 1, 2) === Way(Down, SortedSet(1, 2, 3)))
        assert(Way.makeDown(3, 1, 2) !== Way(Up, SortedSet(1, 2, 3)))
      }

      it("rest of way") {
        assert(restOfWay(Way.makeDown(1, 2, 3), 2) === Way.makeDown(1, 2))
        assert(restOfWay(Way.makeUp(1, 2, 3), 2) === Way.makeUp(2, 3))
        assert(restOfWay(Way.makeUp(1, 2, 3), 4) === Way.makeUp())
        assert(restOfWay(Way.makeUp(), 1) === Way.makeUp())
      }
    }
  }
}
