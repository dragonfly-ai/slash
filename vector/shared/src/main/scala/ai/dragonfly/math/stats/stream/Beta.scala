package ai.dragonfly.math.stats.stream

import ai.dragonfly.math.util.{Demonstrable, OnlineProbDistDemo}

object Beta {
  val demo = OnlineProbDistDemo[ai.dragonfly.math.stats.Beta]("Streaming Beta", ai.dragonfly.math.stats.Beta(3.0, 0.75, 42.0, 69.0), Beta(), 1000000)
}

class Beta extends Online[ai.dragonfly.math.stats.Beta] {
  private val G: Gaussian = Gaussian()

  override def apply(observation: Double, frequency: Double): Beta = {
    G(observation, frequency)
    this
  }

  override def freeze:ai.dragonfly.math.stats.Beta = {
    val scale:Double = G.MAX - G.min
    val μS:Double = (G.μ - G.min) / scale
    val `σ²`:Double = G.`σ²` / (scale * scale)
    val `1-μS`:Double = 1.0 - μS

    val α = μS * (((μS * `1-μS`)/`σ²`) - 1.0)
    val β = `1-μS` * (((μS * `1-μS`)/`σ²`) - 1.0)

    ai.dragonfly.math.stats.Beta(α, β, G.min, G.MAX)
  }

  def μ: Double = freeze.μ
  def `σ²`: Double = freeze.`σ²`
  def σ: Double = freeze.σ

  def random():Double = freeze.random()

  def p(x:Double): Double = freeze.p(x)

  override def min:Double = G.min
  override def MAX:Double = G.MAX

  override def toString: String = {
    val frozen:ai.dragonfly.math.stats.Beta = freeze
    s"stream.Beta(α = ${frozen.α}, β = ${frozen.β}, min = ${frozen.min}, MAX = ${frozen.MAX}, μ = ${frozen.μ}, σ² = ${frozen.`σ²`}, σ = ${frozen.σ}, N = ${G.s0})"
  }

}