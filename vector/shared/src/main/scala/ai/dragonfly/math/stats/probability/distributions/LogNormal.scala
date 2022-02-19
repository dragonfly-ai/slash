package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.*
import examples.*

// https://en.wikipedia.org/wiki/Log-normal_distribution#Generation_and_parameters

object LogNormal {

  val demo = ProbabilityDistributionDemonstration("LogNormal", LogNormal(15.0, 5.0))

  def fromGaussianParameters(Gμ:Double, `Gσ²`: Double): LogNormal = fromGaussian( Gaussian(Gμ, `Gσ²`) )
  def fromGaussian(g: Gaussian): LogNormal = LogNormal(
    Math.exp( g.μ + (g.`σ²` / 2) ),
    (Math.exp(g.`σ²`) - 1) * Math.exp((2 * g.μ) + (g.`σ²`))
  )

  private val c1:Double = Math.sqrt(2.0 * π)
  def p(x:Double, μG:Double, σG: Double):Double = {
    val c2:Double = Math.log(x) - μG
    (1.0 / (x * (σG * c1))) * Math.exp(-0.5 * ((c2*c2) / (σG * σG)))
  }
}

case class LogNormal(μ:Double, `σ²`: Double) extends ContinuousProbabilityDistribution {

  val G:Gaussian = {
    val `μ²`:Double = μ * μ
    Gaussian(
      Math.log( `μ²` / Math.sqrt(`μ²` + `σ²`) ), // transform mean
      Math.log( 1.0 + (`σ²` / `μ²`) ) // transform variance
    )
  }

  lazy val σ: Double = Math.sqrt(`σ²`)

  override def min: Double = 0.0
  override def MAX: Double = Double.PositiveInfinity

  def p(x:Double):Double = LogNormal.p(x, G.μ, G.σ)

  override def random(): Double = {
    Math.exp(G.random())
  }

  override def toString: String = s"LogNormal( μ = $μ, σ² = ${`σ²`}, σ = $σ )" //, gaussian = $gaussian )"
}
