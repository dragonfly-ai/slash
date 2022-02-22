package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.interval.{Domain, Interval}
import ai.dragonfly.math.stats.probability.distributions.Sampleable

// ℕ population size symbol for future reference.

trait ProbabilityDistribution[DOMAIN: Numeric] extends Sampleable[DOMAIN] {
  val `#` = implicitly[Numeric[DOMAIN]]
  import `#`._

  /**
   * Probability Densidy Function: PDF
   * Computes the probability of drawing sample x from this distribution.
   *
   * @param x a sample
   * @return Probability(x)
   */
  def p(x: DOMAIN): Double

  //  /**
  //   * Cumulative Density Function: CDF
  //   * Computes the probability of drawing a sample less than or equal to x from this distribution.
  //   * @param x a sample
  //   * @return Probability(i <= x) for all i
  //   */
  //  TODO: maybe someday.
  //  def cumulative(x:Double):Double

}


trait ParametricProbabilityDistribution[DOMAIN] extends ProbabilityDistribution[DOMAIN] {
  def μ: Double
  def mean: Double = μ

  def `σ²`: Double
  def variance: Double = `σ²`

  def σ: Double
  def standardDeviation: Double = σ
}
