package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.stats.probability.distributions.Sampleable

trait ProbabilityDistribution extends Sampleable[Double] {
  def μ: Double

  def mean: Double = μ

  def `σ²`: Double

  def variance: Double = `σ²`

  def σ: Double

  def standardDeviation: Double = σ

  /**
   * Probability Densidy Function: PDF
   * Computes the probability of drawing sample x from this distribution.
   *
   * @param x a sample
   * @return Probability(x)
   */
  def p(x: Double): Double

  //  /**
  //   * Cumulative Density Function: CDF
  //   * Computes the probability of drawing a sample less than or equal to x from this distribution.
  //   * @param x a sample
  //   * @return Probability(i <= x) for all i
  //   */
  //  TODO: maybe someday.
  //  def cumulative(x:Double):Double

  def min: Double = Double.MinValue

  def MAX: Double = Double.MaxValue
}
