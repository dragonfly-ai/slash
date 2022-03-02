package ai.dragonfly.math.stats.probability.distributions


import ai.dragonfly.math.example.ProbabilityDistributionDemonstration
import ai.dragonfly.math.stats.*
import ai.dragonfly.math.interval.*
import ai.dragonfly.math.interval.Interval
import ai.dragonfly.math.stats.BoundedMean

object PERT {
  val demo = ProbabilityDistributionDemonstration("PERT", PERT(5.0, 6.0, 11.0), DenseHistogramOfContinuousDistribution(11, 5.0, 11.0))
  def apply(min:Double, μ:Double, MAX:Double):PERT = PERT(
    BoundedMean[Double](μ, `[]`[Double](min, MAX), 1.0)
  )

  val domain:Domain[Double] = Domain.ℝ_Double

}

case class PERT(boundedMean:BoundedMean[Double]) extends ParametricProbabilityDistribution[Double] {

  val interval:Interval[Double] = boundedMean.bounds
  override val μ:Double = boundedMean.μ
  override val `σ²`:Double = ((μ - interval.min) * (interval.MAX - μ)) / 7.0
  override lazy val σ:Double = Math.sqrt(`σ²`)

  private lazy val underlying:Beta = Beta.fromPERT(this)

  override def p(x:Double):Double = underlying.p(x)

  override def random(): Double = underlying.random()

  override def toString: String = s"PERT( min = ${interval.min}, μ = $μ, MAX = ${interval.MAX}, σ² = ${`σ²`}, σ = $σ )"

}


case class EstimatedPERT(override val idealized: PERT, override val ℕ̂:Double) extends EstimatedProbabilityDistribution[Double, PERT]{
  override val interval = idealized.interval
  override def toString: String = s"EstimatedPERT(min = ${interval.min}, μ̂ = $μ̂, MAX = ${interval.MAX}, σ̂² = ${`σ̂²`}, σ̂ = $σ̂, ℕ̂ = $ℕ̂)"
}