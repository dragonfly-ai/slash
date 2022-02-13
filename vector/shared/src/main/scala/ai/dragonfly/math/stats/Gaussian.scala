package ai.dragonfly.math.stats

import ai.dragonfly.math.util.ProbDistDemo

import scala.util.Random

object Gaussian {
  val demo:ProbDistDemo = ProbDistDemo("Gaussian", Gaussian(10.0, 42.25))
}

case class Gaussian(μ:Double, `σ²`:Double) extends ProbabilityDistribution {
  lazy val σ:Double = Math.sqrt(`σ²`)
  override def random(): Double = μ + ( Random.nextGaussian() * σ )
  override def toString: String = s"Gaussian(μ = $μ, σ² = ${`σ²`}, σ = $σ)"
  private val s0: Double = 1.0 / (σ * Math.sqrt( 2.0 * Math.PI))
  override def p(x: Double):Double = {
    val s1: Double = (x - μ) / σ
    s0 * Math.exp(-0.5 * (s1*s1))
  }
}
