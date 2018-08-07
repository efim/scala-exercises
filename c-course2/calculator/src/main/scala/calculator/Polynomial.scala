package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      math.pow(b(), 2) - 4*a()*c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val (a_, b_, c_, delta_) = (a(), b(), c(), delta())

      delta_ match {
        case _ if delta_ < 0 => Set()
        case _ if delta_ == 0 => Set(-b_ / 2*a_)
        case _ => {
          val deltaRoot = math.sqrt(delta_)
          Set((-b_ + deltaRoot)/(2*a_), (-b_ - deltaRoot)/(2*a_))
        }
      }
    }
  }
}
