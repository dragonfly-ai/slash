package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.stats.probability.distributions.ProbabilityDistribution
import ai.dragonfly.math.*
import stats.*
import example.*
import interval.*

import scala.util.Random

object Gaussian {
  val g10_42:Gaussian = Gaussian(10.0, 42.0)
  val σ6:Long = Math.ceil(g10_42.σ * 6).toLong
  val demo = ProbabilityDistributionDemonstration( "Gaussian", g10_42, DenseHistogramOfContinuousDistribution(13, g10_42.μ - σ6, g10_42.μ + σ6 ))

  val domain:Domain[Double] = Domain.ℝ_Double
}

case class Gaussian(override val μ:Double, override val `σ²`:Double) extends ParametricProbabilityDistribution[Double]  {

  lazy val σ: Double = Math.sqrt(`σ²`)

  // precomputed constants
  private lazy val `1 / (σ * √(2π))`: Double = 1.0 / (σ * Math.sqrt(2.0 * π))

  override def p(x: Double): Double = {
    val `-(((x-μ)/σ)²)/2`: Double = -squareInPlace((x - μ) / σ) / 2.0
    `1 / (σ * √(2π))` * Math.exp(`-(((x-μ)/σ)²)/2`)
  }

  override def random(): Double = μ + (Random.nextGaussian() * σ)

  override def toString: String = s"Gaussian(μ = $μ, σ² = ${`σ²`}, σ = $σ)"
}

case class EstimatedGaussian(override val interval: Interval[Double], override val idealized: Gaussian, override val ℕ̂:Double) extends EstimatedProbabilityDistribution[Double, Gaussian]{
  override def toString: String = s"GaussianEstimate(min = ${interval.min}, MAX = ${interval.MAX}, μ̂ = $μ̂, σ̂² = ${`σ̂²`}, σ̂ = $σ̂, ℕ̂ = $ℕ̂)"
}