package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.*
import stats.*
import examples.*
import interval.*

object Poisson {
  val p15:Poisson = Poisson(15)
  val σ6:Long = Math.ceil(p15.σ * 6).toLong
  val demo = ProbabilityDistributionDemonstration("Poisson", p15, DenseHistogramOfDiscreteDistribution(15, p15.λ.toLong - σ6, p15.λ.toLong + σ6))
  val domain:Domain[Long] = Domain.ℕ_Long
}

case class Poisson(λ:Double) extends ParametricProbabilityDistribution[Long] {

  def lambda:Double = λ
  override val μ:Double = λ
  override val `σ²`:Double = λ
  lazy val σ:Double = Math.sqrt(λ)

  def p(x:Long):Double = Math.exp( x.toDouble * Math.log(λ) - λ - Math.log(Γ(x.toDouble+1.0)) )

  // Knuth's method:
  override def random(): Long = {
    val L = BigDecimal(Math.pow(Math.E, -λ))
    var k = 0L
    var p = 1.0
    while (p > L) {
      k = k + 1L
      p = p * Math.random()
    }
    k - 1
  }
//Poissonλ = μ = σ² = $λ, σ = √λ = $σ, n = $s0)
  override def toString: String = s"Poisson(λ = μ = σ² = $λ, √λ = $σ)"
}

case class EstimatedPousson(override val interval:Interval[Long], override val idealized: Poisson, override val ℕ̂:Long) extends EstimatedProbabilityDistribution[Long, Poisson]{
  def λ̂:Double = idealized.λ
  def sampleLambda:Double = idealized.λ

  override def toString: String = s"PoissonEstimate(min = ${interval.min}, MAX = ${interval.MAX}, λ̂ = μ̂ = σ̂² = $λ̂, √λ = $σ̂, ℕ̂ = $ℕ̂)"
}