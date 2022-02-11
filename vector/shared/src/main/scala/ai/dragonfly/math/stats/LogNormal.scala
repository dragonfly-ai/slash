package ai.dragonfly.math.stats

import ai.dragonfly.math.util.{Demonstrable, ProbDistDemo}

// https://en.wikipedia.org/wiki/Log-normal_distribution#Generation_and_parameters

object LogNormal {

  val demo: ProbDistDemo = ProbDistDemo("LogNormal", LogNormal(15.0, 5.0), 1000)

  def fromGaussianParameters(mean:Double, variance:Double): LogNormal = fromGaussian( Gaussian(mean, variance) )
  def fromGaussian(gaussian: Gaussian): LogNormal = LogNormal(
    Math.exp( gaussian.mean + (gaussian.variance / 2) ),
    (Math.exp(gaussian.variance) - 1) * Math.exp((2 * gaussian.mean) + (gaussian.variance))
  )

  private val c1:Double = Math.sqrt(2.0 * Math.PI)
  def p(x:Double, gaussianMean:Double, gaussianStandardDeviation: Double):Double = {
    val c2:Double = Math.log(x) - gaussianMean
    (1.0 / (x * (gaussianStandardDeviation * c1))) * Math.exp(-0.5 * ((c2*c2) / (gaussianStandardDeviation * gaussianStandardDeviation)))
  }
}

case class LogNormal(mean:Double, variance: Double) extends ProbabilityDistribution {

  val gaussian:Gaussian = {
    val mean2:Double = mean * mean
    Gaussian(
      Math.log( mean2 / Math.sqrt(mean2 + variance) ), // transform mean
      Math.log( 1.0 + (variance / mean2) ) // transform variance
    )
  }

  lazy val standardDeviation: Double = Math.sqrt(variance)


  def p(x:Double):Double = LogNormal.p(x, gaussian.mean, gaussian.standardDeviation)

  override def random(): Double = {
    Math.exp(gaussian.random())
  }

  override def toString: String = s"LogNormal( μ = $mean, σ² = $variance, σ = $standardDeviation )" //, gaussian = $gaussian )"
}
