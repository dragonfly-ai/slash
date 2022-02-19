package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.stats.probability.distributions.ProbabilityDistribution
import ai.dragonfly.math.*
import examples.*

import scala.util.Random

object Gaussian {
  val demo = ProbabilityDistributionDemonstration("Gaussian", Gaussian(10.0, 42.25))
}

case class Gaussian(μ:Double, `σ²`:Double) extends ContinuousProbabilityDistribution {

  override def min: Double = Double.NegativeInfinity
  override def MAX: Double = Double.PositiveInfinity

  lazy val σ:Double = Math.sqrt(`σ²`)

  override def toString: String = s"Gaussian(μ = $μ, σ² = ${`σ²`}, σ = $σ)"
  // precomputed constants
  private lazy val `1 / (σ * √(2π))`: Double = 1.0 / (σ * Math.sqrt( 2.0 * π))

  override def p(x: Double):Double = {
    var `-(((x-μ)/σ)²)/2`: Double = {
      val t = (x - μ) / σ
      -((t * t) / 2.0)
    }
    `1 / (σ * √(2π))` * Math.exp(`-(((x-μ)/σ)²)/2`)
  }

  override def random(): Double = μ + ( Random.nextGaussian() * σ )
}
