package ai.dragonfly.math.stats.stream

import ai.dragonfly.math.stats.Sampleable
import ai.dragonfly.math.util.{Demonstrable, OnlineProbDistDemo}


object LogNormal {
  val demo = OnlineProbDistDemo[ai.dragonfly.math.stats.LogNormal]("Streaming LogNormal", ai.dragonfly.math.stats.LogNormal(69, 21), LogNormal(), 1000)
}

class LogNormal() extends Online[ai.dragonfly.math.stats.LogNormal] {
  val gaussian:Gaussian = Gaussian()
  def apply(observation: Double, frequency: Double = 1.0):LogNormal = {
    gaussian.apply(Math.log(observation), frequency)
    this
  }

  override def random(): Double = {
    Math.exp(gaussian.random())
  }

  def min:Double = Math.exp( gaussian.min )
  def max:Double = Math.exp( gaussian.max )

  def mean: Double = Math.exp( gaussian.mean + (gaussian.variance / 2) )

  def variance: Double = (Math.exp(gaussian.variance) - 1) * Math.exp((2 * gaussian.mean) + (gaussian.variance))

  def standardDeviation: Double = Math.sqrt(variance)

  def p(x:Double):Double = ai.dragonfly.math.stats.LogNormal.p(x, gaussian.mean, gaussian.standardDeviation)

  def freeze:ai.dragonfly.math.stats.LogNormal = ai.dragonfly.math.stats.LogNormal(mean, variance)

  override def toString: String = s"stream.LogNormal( min = $min, MAX = $max, μ = $mean, σ² = $variance, σ = $standardDeviation )" // ${gaussian.toString} )"
}
