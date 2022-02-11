package actor.domain

final case class Ride(start: Int, end: Int) {
  val direction: Direction = if (start > end) Down else Up
  val floors: List[Int] = List(start, end)

  override def toString = s"$direction($start, $end)"
}
