package ai.dragonfly.math.stats

import ai.dragonfly.math.util.Demonstrable

import scala.util.Random

object Gaussian extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val gaussian:Gaussian = Gaussian(10.0, 42.25) // standardDeviation 6.5
    val histogram:DenseDiscreteHistogram = new DenseDiscreteHistogram(60, -20.0, 40.0)
    for(i <- 0 until 1000){ histogram(gaussian.random()) }
    sb.append(histogram)
  }

  override def name: String = "Gaussian"
}

case class Gaussian(mean:Double, variance:Double) extends Sampleable[Double] {
  lazy val standardDeviation:Double = Math.sqrt(variance)
  override def random(): Double = mean + ( Random.nextGaussian() * standardDeviation )
  override def toString: String = s"stream.Gaussian(μ = $mean, σ = $variance, √σ = $standardDeviation)"
}
