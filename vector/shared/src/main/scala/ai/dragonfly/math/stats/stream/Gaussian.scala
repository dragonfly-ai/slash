package ai.dragonfly.math.stats.stream

import ai.dragonfly.math.stats.Sampleable
import ai.dragonfly.math.util.Demonstrable

import scala.util.Random

object Gaussian extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val mean:Double = 25.0
    val standardDeviation:Double = 10.0
    sb.append(s"\tGenerating 1000 samples with Mean:$mean Standard Deviation: $standardDeviation")
    val g:ai.dragonfly.math.stats.Gaussian = ai.dragonfly.math.stats.Gaussian(mean, standardDeviation)
    val sg:Gaussian = new Gaussian()
    for (i <- 0 until 1000) {
      sg(g.random())
    }
    sb.append(s"\tEstimated Mean: ${sg.average} Estimated Standard Deviation: ${sg.standardDeviation}")
  }

  override def name: String = "Streaming Gaussian"
}

class Gaussian extends Sampleable[Double] {

  private var minObservation = Double.MaxValue
  private var maxObservation = Double.MinValue

  private var s0 = 0.0
  private var s1 = 0.0
  private var s2 = 0.0

  def apply(observation: Double, frequency: Double = 1.0): Unit = {
    maxObservation = Math.max(observation, maxObservation)
    minObservation = Math.min(observation, minObservation)

    s0 = s0 + frequency
    s1 = s1 + observation * frequency
    s2 = s2 + observation * observation * frequency
  }

  def min:Double = minObservation
  def max:Double = maxObservation

  @inline def average:Double = s1 / s0

  @inline def variance:Double = (s0 * s2 - s1 * s1) / (s0 * (s0 - 1.0))

  def standardDeviation:Double = Math.sqrt(variance)

  override def toString: String = s"Min: $min Max: $max Avg: $average Variance: $variance STDV: $standardDeviation Sample size: $s0"

  override def random(): Double = average + ( Random.nextGaussian() * standardDeviation )
}
