package ai.dragonfly.math.stats

import ai.dragonfly.math.util.ProbDistDemo

import scala.util.Random

object Gaussian {
  val demo:ProbDistDemo = ProbDistDemo("Gaussian", Gaussian(10.0, 42.25))
}

case class Gaussian(mean:Double, variance:Double) extends ProbabilityDistribution {
  lazy val standardDeviation:Double = Math.sqrt(variance)
  override def random(): Double = mean + ( Random.nextGaussian() * standardDeviation )
  override def toString: String = s"Gaussian(μ = $mean, σ² = $variance, σ = $standardDeviation)"
  private val s0: Double = 1.0 / (standardDeviation * Math.sqrt( 2.0 * Math.PI))
  override def p(x: Double):Double = {
    val s1: Double = (x - mean) / standardDeviation
    s0 * Math.exp(-0.5 * (s1*s1))
  }
}
