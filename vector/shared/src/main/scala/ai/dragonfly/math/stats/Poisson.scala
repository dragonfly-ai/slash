package ai.dragonfly.math.stats

import ai.dragonfly.math.util.{Demonstrable, ProbDistDemo, Γ}

object Poisson {
  val demo: ProbDistDemo = ProbDistDemo("Poisson", Poisson(15))
}

case class Poisson(λ:Double) extends ProbabilityDistribution {
  def lambda:Double = λ
  override val μ:Double = λ
  override val `σ²`:Double = λ
  lazy val σ:Double = Math.sqrt(λ)

  override val min:Double = 0.0

  def p(x:Double):Double = Math.exp( x * Math.log(λ) - λ - Math.log(Γ(x+1)) )

  // Knuth's method:
  override def random(): Double = {
    val e = Math.E
    val L = BigDecimal(Math.pow(e, -λ))
    var k = 0
    var p = 1.0
    while (p > L) {
      k = k + 1
      p = p * Math.random()
    }
    k - 1
  }
//Poissonλ = μ = σ² = $λ, σ = √λ = $σ, n = $s0)
  override def toString: String = s"Poisson(λ = μ = σ² = $λ, √λ = $σ)"
}
