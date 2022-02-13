package ai.dragonfly.math.stats.stream

import ai.dragonfly.math.util.{Demonstrable, OnlineProbDistDemo}

import scala.util.Random

object Gaussian {
  val demo = OnlineProbDistDemo[ai.dragonfly.math.stats.Gaussian]("Streaming Gaussian", ai.dragonfly.math.stats.Gaussian(42.0, 7.0), Gaussian(), 1000)
}

class Gaussian extends Online[ai.dragonfly.math.stats.Gaussian] {

  private var minObservation = Double.MaxValue
  private var maxObservation = Double.MinValue

  private var s0 = 0.0
  private var s1 = 0.0
  private var s2 = 0.0

  def apply(observation: Double, frequency: Double = 1.0):Gaussian = {
    minObservation = Math.min(observation, minObservation)
    maxObservation = Math.max(observation, maxObservation)

    s0 = s0 + frequency
    s1 = s1 + observation * frequency
    s2 = s2 + observation * observation * frequency

    this
  }

  override def min:Double = minObservation
  override def MAX:Double = maxObservation

  def sampleSize:Double = s0

  inline def μ:Double = s1 / s0

  /**
   * σ
   * @return
   */
  inline def `σ²`:Double = (s0 * s2 - s1 * s1) / (s0 * (s0 - 1.0))

  /**
   * 	√σ
   * @return
   */
  inline def σ:Double = Math.sqrt(`σ²`)

  def p(x:Double):Double = freeze.p(x)

  override def toString: String = s"stream.Gaussian(min = $min, MAX = $MAX, μ = $μ, σ² = ${`σ²`}, σ = $σ, N = $s0)"

  override def random(): Double = μ + ( Random.nextGaussian() * σ )

  def freeze:ai.dragonfly.math.stats.Gaussian = ai.dragonfly.math.stats.Gaussian(μ, `σ²`)
}
