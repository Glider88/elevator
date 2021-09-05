package test.catz.domain

import org.scalatest.funspec.AnyFunSpec
import test.CustomAsserts
import _root_.catz.domain.DoubleComparison._
import org.scalatest.prop.Tables.Table
import org.scalatest.prop.TableDrivenPropertyChecks._

class DoubleComparisonTest extends AnyFunSpec with CustomAsserts {
  val lessThan =
    Table(
      ("double", "int"),
      (0.00, 1),
      (0.94, 1),
    )

  val equal =
    Table(
      ("double", "int"),
      (0.95, 1),
      (1.00, 1),
      (1.04, 1),
    )

  val greatedThan =
    Table(
      ("double", "int"),
      (1.05, 1),
      (2.00, 1),
    )

  describe("Float and Int custom comparison") {
    it("eq") {
      forAll (lessThan) { (double: Double, int: Int) =>
        assertNot(double eq int)
        assertNot(int eq double)
        assertNot(double eq int.toDouble)
      }

      forAll (equal) { (double: Double, int: Int) =>
        assert(double eq int)
        assert(int eq double)
        assert(double eq int.toDouble)
      }

      forAll (greatedThan) { (double: Double, int: Int) =>
        assertNot(double eq int)
        assertNot(int eq double)
        assertNot(double eq int.toDouble)
      }
    }

    it("lt") {
      forAll (lessThan) { (double: Double, int: Int) =>
        assert(double lt int)
        assert(double lt int.toDouble)
      }

      forAll (greatedThan) { (double: Double, int: Int) =>
        assert(int lt double)
      }

      forAll (equal) { (double: Double, int: Int) =>
        assertNot(double lt int)
        assertNot(int lt double)
        assertNot(double lt int.toDouble)
      }

      forAll (greatedThan) { (double: Double, int: Int) =>
        assertNot(double lt int)
        assertNot(double lt int.toDouble)
      }

      forAll (lessThan) { (double: Double, int: Int) =>
        assertNot(int lt double)
      }
    }

    it("lte") {
      forAll (lessThan) { (double: Double, int: Int) =>
        assert(double lte int)
        assert(double lte int.toDouble)
      }

      forAll (greatedThan) { (double: Double, int: Int) =>
        assert(int lte double)
      }

      forAll (equal) { (double: Double, int: Int) =>
        assert(double lte int)
        assert(int lte double)
        assert(double lte int.toDouble)
      }

      forAll (greatedThan) { (double: Double, int: Int) =>
        assertNot(double lte int)
        assertNot(double lte int.toDouble)
      }

      forAll (lessThan) { (double: Double, int: Int) =>
        assertNot(int lte double)
      }
    }

    it("gt") {
      forAll (lessThan) { (double: Double, int: Int) =>
        assertNot(double gt int)
        assertNot(double gt int.toDouble)
      }

      forAll (greatedThan) { (double: Double, int: Int) =>
        assertNot(int gt double)
      }

      forAll (equal) { (double: Double, int: Int) =>
        assertNot(double gt int)
        assertNot(int gt double)
        assertNot(double gt int.toDouble)
      }

      forAll (greatedThan) { (double: Double, int: Int) =>
        assert(double gt int)
        assert(double gt int.toDouble)
      }

      forAll (lessThan) { (double: Double, int: Int) =>
        assert(int gt double)
      }
    }

    it("gte") {
      forAll (lessThan) { (double: Double, int: Int) =>
        assertNot(double gte int)
        assertNot(double gte int.toDouble)
      }

      forAll (greatedThan) { (double: Double, int: Int) =>
        assertNot(int gte double)
      }

      forAll (equal) { (double: Double, int: Int) =>
        assert(double gte int)
        assert(int gte double)
        assert(double gte int.toDouble)
      }

      forAll (greatedThan) { (double: Double, int: Int) =>
        assert(double gte int)
        assert(double gte int.toDouble)
      }

      forAll (lessThan) { (double: Double, int: Int) =>
        assert(int gte double)
      }
    }
  }
}
