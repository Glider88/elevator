package catz.domain

object DoubleComparison {

  def round(number: Double): Double =
    BigDecimal(number).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble

  implicit class DoubleCompare(x: Double) {
    def eq(y: Int): Boolean = round(x) == y
    def neq(y: Int): Boolean = round(x) != y
    def gt(y: Int): Boolean = round(x) > y
    def gte(y: Int): Boolean = (x eq y) || (round(x) > y)
    def lt(y: Int): Boolean = round(x) < y
    def lte(y: Int): Boolean = (x eq y) || (round(x) < y)

    def eq(y: Double): Boolean = round(x) == round(y)
    def neq(y: Double): Boolean = round(x) != round(y)
    def gt(y: Double): Boolean = round(x) > round(y)
    def gte(y: Double): Boolean = (x eq y) || (round(x) > round(y))
    def lt(y: Double): Boolean = round(x) < round(y)
    def lte(y: Double): Boolean = (x eq y) || (round(x) < round(y))
  }

  implicit class IntCompare(x: Int) {
    def eq(y: Double): Boolean = x == round(y)
    def neq(y: Double): Boolean = x != round(y)
    def gt(y: Double): Boolean = x > round(y)
    def gte(y: Double): Boolean = (x eq y) || (x > round(y))
    def lt(y: Double): Boolean = x < round(y)
    def lte(y: Double): Boolean = (x eq y) || (x < round(y))
  }
}
