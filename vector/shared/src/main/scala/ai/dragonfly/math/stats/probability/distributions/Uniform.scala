package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.*
import stats.*
import ai.dragonfly.math.interval.*
import ai.dragonfly.math.example.ProbabilityDistributionDemonstration

import scala.language.postfixOps

object Uniform {
  val demo = ProbabilityDistributionDemonstration("Uniform", Uniform(5.0, 11.0), DenseHistogramOfContinuousDistribution(7, 5, 11))
  def apply(b1:Double, b2:Double): Uniform = {
    Uniform(`[]`[Double](Math.min(b1, b2), Math.max(b1, b2)))
  }
  lazy val domain:Domain[Double] = Domain.ℝ_Double
}

case class Uniform(interval:Interval[Double]) extends ParametricProbabilityDistribution[Double] {

  private lazy val `MAX-min`:Double = interval.MAX - interval.min
  private lazy val `1 / (MAX-min)`:Double = 1.0 / `MAX-min`

  override val μ:Double = (interval.min + interval.MAX) / 2.0

  override lazy val `σ²`:Double = ( `MAX-min` * `MAX-min` ) / 12.0
  override lazy val σ:Double = Math.sqrt(`σ²`)

  override def p(x:Double):Double = if (interval.contains(x)) `1 / (MAX-min)` else 0.0

  override def random(): Double = interval.min + (Math.random() * (interval.MAX - interval.min))

  override def toString: String = s"Uniform( min = ${interval.min}, μ = $μ, MAX = ${interval.MAX}, σ² = ${`σ²`}, σ = $σ )"

}


case class EstimatedUniform(override val idealized: Uniform, override val ℕ:Double) extends EstimatedProbabilityDistribution[Double, Uniform]{

  override val interval:Interval[Double] = idealized.interval

  override def toString: String = s"UniformEstimate(min = ${interval.min}, μ = $μ, MAX = ${interval.MAX}, σ² = ${`σ²`}, σ = $σ, ℕ = $ℕ)"
}
