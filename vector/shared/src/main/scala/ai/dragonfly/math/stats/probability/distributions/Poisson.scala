package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.stats.probability.distributions.ProbabilityDistribution
import ai.dragonfly.math.*
import examples.*

object Poisson {
  val demo = ProbabilityDistributionDemonstration("Poisson", Poisson(15))
}

case class Poisson(λ:Double) extends DiscreteProbabilityDistribution {

  def min: Long = 0L
  def MAX: Long = Long.MaxValue

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
