package ai.dragonfly.math.stats

// https://en.wikipedia.org/wiki/Log-normal_distribution#Generation_and_parameters

object LogNormal {
  def fromGaussianParameters(mean:Double, variance:Double): LogNormal = LogNormal( Gaussian(mean, variance) )
}

case class LogNormal(gaussian: Gaussian) extends Sampleable[Double] {

  lazy val mean: Math.exp( gaussian.mean + (gaussian.variance / 2) )
  lazy val variance: Double = (Math.exp(gaussian.variance) - 1) * Math.exp((2 * gaussian.mean) + (gaussian.variance))
  lazy val standardDeviation: Double = Math.sqrt(variance)

  override def random(): Double = {
    Math.exp(gaussian.random())
  }

  override def toString: String = s"LogNormal( ${gaussian.toString} )"
}
