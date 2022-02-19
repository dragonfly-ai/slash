package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.stats.probability.distributions
import ai.dragonfly.math.examples.*

object Beta {
  val demo = ContinuousOnlineProbDistDemo("Streaming Beta", distributions.Beta(3.0, 0.75, 42.0, 69.0), Beta(), 1000000)
}

class Beta extends OnlineContinuous {
  private val G: Gaussian = Gaussian()

  override def apply(observation: Double, frequency: Double): Beta = {
    G(observation, frequency)
    this
  }

  override def freeze:distributions.Beta = {
    val scale:Double = G.MAX - G.min
    val μS:Double = (G.μ - G.min) / scale
    val `σ²`:Double = G.`σ²` / (scale * scale)
    val `1-μS`:Double = 1.0 - μS

    val α = μS * (((μS * `1-μS`)/`σ²`) - 1.0)
    val β = `1-μS` * (((μS * `1-μS`)/`σ²`) - 1.0)

    distributions.Beta(α, β, G.min, G.MAX)
  }

  override def min:Double = G.min
  override def MAX:Double = G.MAX

  override def n:Double = G.n

  def μ: Double = freeze.μ
  def `σ²`: Double = freeze.`σ²`
  def σ: Double = freeze.σ

  def random():Double = freeze.random()

  def p(x:Double): Double = freeze.p(x)

  override def toString: String = {
    val frozen:distributions.Beta = freeze
    s"stream.Beta(α = ${frozen.α}, β = ${frozen.β}, min = ${frozen.min}, MAX = ${frozen.MAX}, μ = ${frozen.μ}, σ² = ${frozen.`σ²`}, σ = ${frozen.σ}, N = ${G.sampleSize})"
  }

}