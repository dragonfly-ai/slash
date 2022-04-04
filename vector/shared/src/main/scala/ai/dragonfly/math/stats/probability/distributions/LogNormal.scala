package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.*
import stats.*
import interval.*
import example.*
import Constant.π

// https://en.wikipedia.org/wiki/Log-normal_distribution#Generation_and_parameters

object LogNormal {

  val ln15_5:LogNormal = LogNormal(15.0, 5.0)
  val demo = ProbabilityDistributionDemonstration("LogNormal", ln15_5, DenseHistogramOfContinuousDistribution(21, 1, ln15_5.μ + (6.0*ln15_5.σ)))

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

  val domain:Domain[Double] = Domain.`ℝ+_Double`
}

case class LogNormal(override val μ:Double, override val `σ²`: Double) extends ParametricProbabilityDistribution[Double] {

  val G:Gaussian = {
    val `μ²`:Double = μ * μ
    Gaussian(
      Math.log( `μ²` / Math.sqrt(`μ²` + `σ²`) ), // transform mean
      Math.log( 1.0 + (`σ²` / `μ²`) ) // transform variance
    )
  }

  lazy val σ: Double = Math.sqrt(`σ²`)

  def p(x:Double):Double = LogNormal.p(x, G.μ, G.σ)

  override def random(): Double = {
    Math.exp(G.random())
  }

  override def toString: String = s"LogNormal( μ = $μ, σ² = ${`σ²`}, σ = $σ )"
}


case class EstimatedLogNormal(override val interval:Interval[Double], override val idealized: LogNormal, override val ℕ:Double) extends EstimatedProbabilityDistribution[Double, LogNormal]{
  override def toString: String = s"LogNormalEstimate(min = ${interval.min}, MAX = ${interval.MAX}, μ = $μ, σ² = ${`σ²`}, σ = $σ, ℕ = $ℕ)"
}