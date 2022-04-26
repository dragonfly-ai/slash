package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.stats.probability.distributions.ProbabilityDistribution
import ai.dragonfly.math.*
import Constant.`√(2π)`
import stats.*
import example.*
import interval.*


object Gaussian {
  val g10_42:Gaussian = Gaussian(10.0, 42.0)
  val σ6:Long = Math.ceil(g10_42.σ * 6).toLong
  val demo = ProbabilityDistributionDemonstration( "Gaussian", g10_42, DenseHistogramOfContinuousDistribution(13, g10_42.μ - σ6, g10_42.μ + σ6 ))

  val domain:Domain[Double] = Domain.ℝ_Double
}

case class Gaussian(override val μ:Double, override val `σ²`:Double) extends ParametricProbabilityDistribution[Double]  {

  lazy val σ: Double = Math.sqrt(`σ²`)

  // precomputed constants
  private lazy val `-1/(2σ²)`: Double = -1.0 / (2.0 * `σ²`)
  private lazy val `1/σ√(2π)`: Double = 1.0 / (σ * `√(2π)`)

  override def p(x: Double): Double = p2(squareInPlace(x - μ))
  def p2(magSquared: Double): Double = `1/σ√(2π)` * Math.exp(`-1/(2σ²)` * magSquared)

  override def random(r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom): Double = μ + (r.nextGaussian() * σ)

  override def toString: String = s"Gaussian(μ = $μ, σ² = ${`σ²`}, σ = $σ)"
}

case class EstimatedGaussian(override val interval: Interval[Double], override val idealized: Gaussian, override val ℕ:Double) extends EstimatedProbabilityDistribution[Double, Gaussian]{
  override def toString: String = s"GaussianEstimate(min = ${interval.min}, MAX = ${interval.MAX}, μ = $μ, σ² = ${`σ²`}, σ = $σ, ℕ = $ℕ)"
}