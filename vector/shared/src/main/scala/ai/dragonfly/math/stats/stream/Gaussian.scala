package ai.dragonfly.math.stats.stream

import ai.dragonfly.math.stats.Sampleable
import ai.dragonfly.math.util.Demonstrable

import scala.util.Random

object Gaussian extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val mean:Double = 25.0
    val variance:Double = 100.0
    val idealGaussian:ai.dragonfly.math.stats.Gaussian = ai.dragonfly.math.stats.Gaussian(mean, variance)
    sb.append(s"Populate stream.Gaussian by sampling from: $idealGaussian")
    val sg:Gaussian = new Gaussian()
    for (i <- 0 until 1000) { sg(idealGaussian.random()) }
    sb.append(sg)
  }

  override def name: String = "Streaming Gaussian"
}

class Gaussian extends Sampleable[Double] {

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

  def min:Double = minObservation
  def max:Double = maxObservation

  def sampleSize:Double = s0

  @inline def mean:Double = s1 / s0

  /**
   * σ
   * @return
   */
  @inline def variance:Double = (s0 * s2 - s1 * s1) / (s0 * (s0 - 1.0))

  /**
   * 	√σ
   * @return
   */
  @inline def standardDeviation:Double = Math.sqrt(variance)

  override def toString: String = s"stream.Gaussian(min = $min, max = $max, μ = $mean, σ² = $variance, σ = $standardDeviation, n = $s0)"

  override def random(): Double = mean + ( Random.nextGaussian() * standardDeviation )
}
