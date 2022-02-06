package ai.dragonfly.math.stats.stream

import ai.dragonfly.math.stats.Sampleable

class LogNormal(gaussian:Gaussian = Gaussian()) extends Sampleable[Double] {
  def apply(observation: Double, frequency: Double = 1.0):LogNormal = {
    gaussian.apply(Math.log(observation), frequency)
    this
  }

  override def random(): Double = {
    Math.exp(gaussian.random())
  }

  def mean: Double = Math.exp( gaussian.mean + (gaussian.variance / 2) )

  def variance: Double = (Math.exp(gaussian.variance) - 1) * Math.exp((2 * gaussian.mean) + (gaussian.variance))

  def standardDeviation: Double = Math.sqrt(variance)

  override def toString: String = s"LogNormal( ${gaussian.toString} )"
}
