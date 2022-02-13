package ai.dragonfly.math.stats.stream

import ai.dragonfly.math.util.{Demonstrable, OnlineProbDistDemo}


object LogNormal {
  val demo = OnlineProbDistDemo[ai.dragonfly.math.stats.LogNormal]("Streaming LogNormal", ai.dragonfly.math.stats.LogNormal(69, 21), LogNormal(), 1000)
}

class LogNormal() extends Online[ai.dragonfly.math.stats.LogNormal] {
  val G:Gaussian = Gaussian()
  def apply(observation: Double, frequency: Double = 1.0):LogNormal = {
    G.apply(Math.log(observation), frequency)
    this
  }

  override def random(): Double = {
    Math.exp(G.random())
  }

  override def min:Double = Math.exp( G.min )
  override def MAX:Double = Math.exp( G.MAX )

  def μ: Double = Math.exp( G.μ + (G.`σ²` / 2) )

  def `σ²`: Double = (Math.exp(G.`σ²`) - 1) * Math.exp((2 * G.μ) + (G.`σ²`))

  def σ: Double = Math.sqrt(`σ²`)

  def p(x:Double):Double = ai.dragonfly.math.stats.LogNormal.p(x, G.μ, G.σ)

  def freeze:ai.dragonfly.math.stats.LogNormal = ai.dragonfly.math.stats.LogNormal(μ, `σ²`)

  override def toString: String = s"stream.LogNormal( min = $min, MAX = $MAX, μ = $μ, σ² = ${`σ²`}, σ = $σ, N = ${G.sampleSize})" // ${gaussian.toString} )"
}
