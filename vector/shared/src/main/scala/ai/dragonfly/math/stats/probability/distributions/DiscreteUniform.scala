package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.*
import stats.*
import ai.dragonfly.math.interval.*
import ai.dragonfly.math.examples.ProbabilityDistributionDemonstration

import scala.language.postfixOps

object DiscreteUniform {
  val demo = ProbabilityDistributionDemonstration(
    "Uniform",
    DiscreteUniform(5, 15),
    DenseHistogramOfDiscreteDistribution(7, 5, 15)
  )
  def apply(b1:Long, b2:Long): DiscreteUniform = {
    DiscreteUniform(`[]`[Long](Math.min(b1, b2), Math.max(b1, b2)))
  }
  val domain:Domain[Long] = Domain.ℤ_Long
}

case class DiscreteUniform(interval:Interval[Long]) extends ParametricProbabilityDistribution[Long] {

  private lazy val `MAX-min`:Double = interval.MAX - interval.min.toDouble
  private lazy val `1 / (MAX-min)`:Double = 1.0 / `MAX-min`

  override val μ = (interval.min + interval.MAX).toDouble / 2.0

  override lazy val `σ²`:Double = ( (`MAX-min` * `MAX-min`) - 1.0 ) / 12.0
  override lazy val σ:Double = Math.sqrt(`σ²`)


  override def p(x:Long):Double = if (interval.contains(x)) `1 / (MAX-min)` else 0.0

  private val r = scala.util.Random()
  override def random(): Long = interval.min + r.nextLong(interval.MAX - interval.min)

  override def toString: String = s"DiscreteUniform( min = ${interval.min}, μ = $μ, MAX = ${interval.MAX}, σ² = ${`σ²`}, σ = $σ )"

}


case class EstimatedDiscreteUniform(override val interval: Interval[Long], override val idealized: DiscreteUniform, override val ℕ̂:Long) extends EstimatedProbabilityDistribution[Long, DiscreteUniform]{
  override def toString: String = s"DiscreteUniformEstimate(min = ${interval.min}, μ̂ = $μ̂, MAX = ${interval.MAX}, σ̂² = ${`σ̂²`}, σ̂ = $σ̂, ℕ̂ = $ℕ̂)"
}