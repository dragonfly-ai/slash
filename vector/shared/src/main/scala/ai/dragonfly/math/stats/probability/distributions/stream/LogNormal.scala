package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.stats.probability.distributions
import ai.dragonfly.math.util.{Demonstrable, OnlineProbDistDemo}


object LogNormal {
  val demo = OnlineProbDistDemo[distributions.LogNormal]("Streaming LogNormal", distributions.LogNormal(69, 21), LogNormal(), 1000)
}

class LogNormal() extends Online[distributions.LogNormal] {
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

  def p(x:Double):Double = ai.dragonfly.math.stats.probability.distributions.LogNormal.p(x, G.μ, G.σ)

  def freeze:distributions.LogNormal = distributions.LogNormal(μ, `σ²`)

  override def toString: String = s"stream.LogNormal( min = $min, MAX = $MAX, μ = $μ, σ² = ${`σ²`}, σ = $σ, N = ${G.sampleSize})" // ${gaussian.toString} )"
}
