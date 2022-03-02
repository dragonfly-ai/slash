package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.interval.Interval

trait EstimatedProbabilityDistribution[DOMAIN: Numeric, PPD <: ParametricProbabilityDistribution[DOMAIN]] {
  val idealized: PPD

  def ℕ:DOMAIN
  def sampleSize:DOMAIN = ℕ

  def interval:Interval[DOMAIN]

  def μ: Double = idealized.μ
  def sampleMean: Double = μ

  def `σ²`: Double = idealized.`σ²`
  def sampleVariance: Double = `σ²`

  def σ: Double = idealized.σ
  def sampleStandardDeviation: Double = σ

  def p(x: DOMAIN): Double = idealized.p(x)

  def random(): DOMAIN = idealized.random()
}