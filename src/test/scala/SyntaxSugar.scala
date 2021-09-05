package test

import _root_.catz.domain.DomainCore._

trait SyntaxSugar {
  sealed trait W
  case class U(floors: Int*) extends W
  case class D(floors: Int*) extends W

  def R(floor: Double, ways: W*): Route = {
    def way(w: W) = w match {
      case u: U => Some(Way.makeUp(u.floors: _*))
      case d: D => Some(Way.makeDown(d.floors: _*))
    }

    (ways.lift(0), ways.lift(1), ways.lift(2)) match {
      case (Some(f), Some(b), Some(t)) => Route(floor, way(f), way(b), way(t))
      case (Some(f), Some(b), None) => Route(floor, way(f), way(b), None)
      case (Some(f), None, None) => Route(floor, way(f), None, None)
      case _ => Route(floor, None, None, None)
    }
  }
}
