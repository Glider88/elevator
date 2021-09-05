package test

trait CustomAsserts {
  def assertNot(condition: Boolean) = assert(condition == false)
}
