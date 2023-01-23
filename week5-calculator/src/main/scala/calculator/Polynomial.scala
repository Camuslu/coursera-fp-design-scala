package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    new Signal.Var(b()*b() - 4*a()*c())

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    new Signal.Var(
      if (delta() > 0) {
        Set((-1*b() + Math.sqrt(delta()))/(2*a()), (-1*b() - Math.sqrt(delta()))/(2*a()))
      } else if (delta() == 0) {
        Set(-1*b()/(2*a()))
      } else {
        Set[Double]()
      }
    )
