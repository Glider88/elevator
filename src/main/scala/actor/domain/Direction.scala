package actor.domain

sealed trait Direction {
  def reversed: Direction
}

final case object Up extends Direction {
  def reversed: Direction = Down
}

final case object Down extends Direction {
  def reversed: Direction = Up
}
