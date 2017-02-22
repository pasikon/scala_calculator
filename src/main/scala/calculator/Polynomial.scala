package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(b()*b() - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var(computeDelta(a, b, c)() match {
      case d: Double if d > 0 => Set((-b() + d) / 2 * a()) + (-b() - d) / 2 * a()
      case d: Double if d == 0 => Set((-b() + d) / 2 * a())
      case d: Double if d < 0 => Set()
    })
  }
}
