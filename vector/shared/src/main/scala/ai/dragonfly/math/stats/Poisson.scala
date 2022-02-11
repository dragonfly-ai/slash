package ai.dragonfly.math.stats

import ai.dragonfly.math.util.{Demonstrable, ProbDistDemo, gamma}

object Poisson {
  val demo: ProbDistDemo = ProbDistDemo("Poisson", Poisson(15), 1000)
}

case class Poisson(lambda:Double) extends ProbabilityDistribution {
  def mean:Double = lambda
  def variance:Double = lambda
  lazy val stdDev:Double = Math.sqrt(lambda)
  def standardDeviation:Double = stdDev

  def p(x:Double):Double = Math.exp( x * Math.log(mean) - mean - Math.log(gamma(x+1)) )

  // Knuth's method:
  override def random(): Double = {
    val e = Math.E
    val L = BigDecimal(Math.pow(e, -lambda))
    var k = 0
    var p = 1.0
    while (p > L) {
      k = k + 1
      p = p * Math.random()
    }
    k - 1
  }

  override def toString: String = s"Poisson(λ = $lambda, √λ = $standardDeviation)"
}
