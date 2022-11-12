package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.*
import stats.*
import ai.dragonfly.math.Factorial.!
import ai.dragonfly.math.interval.*
import Interval.*

import scala.language.postfixOps

object Binomial {
  lazy val domain:Domain[Long] = Domain.ℕ_Long
}

/**
 * Binomial Distribution: https://en.wikipedia.org/wiki/Binomial_distribution
 *
 * @param n number of trials in process or experiment with two possible outcomes, i.e. a boolean valued random function.
 * @param P probability of a successful outcome on any single trial.
 */
case class Binomial(n:Long, P:Double) extends ParametricProbabilityDistribution[Long] {

  /**
   * `1-p` is the probability of failure.
   */
  val `1-p`:Double = 1.0 - P

  override val μ: Double = n * P
  override val `σ²`: Double = n * P * `1-p`
  override val σ: Double = Math.sqrt(`σ²`)

  lazy val `n!`:BigInt = n!

  /**
   * A true Binomial Distribution has a Probability Mass Function PMF(k), with a meaningful domain of:
   *   n₀ ⊆ ℕ₀ where n₀ = {0, 1, 2, ..., n} and ℕ₀ = ℕ* ∪ {0} = {0, 1, 2, ... ∞}: the natural numbers and 0.
   * and ∀ k < 0 or k > n, PMF(k) = 0.

   * @param x a specific number of successful outcomes given n trials.
   *  @return Probability of x successes given n trials.
   */

  override def p(k: Long): Double = {
    if (k > n || k <= 0) 0.0
    else {
      val `k!`:BigInt = k!
      val `n-k`:Long = n - k
      (`n!`/ (`k!` * (`n-k`!))).toDouble * Math.pow(P, k.toDouble) * Math.pow(1.0 - P, `n-k`.toDouble)
    }
  }

  /**
   * Naive implementation not suitable for large N.
   * @return a random number of successes.
   */
  override def random(r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom): Long = {
    var successCount = 0L
    for (i <- 0L until n) {
      if (r.nextDouble() < P) successCount = successCount + 1L
    }
    successCount
  }

  override def toString: String = s"Binomial(n = $n, P = $P, μ = $μ, σ² = ${`σ²`}, σ = $σ)"

}


case class EstimatedBinomial(override val interval:Interval[Long], override val idealized: Binomial, override val ℕ:Long) extends EstimatedProbabilityDistribution[Long, Binomial]{
  def n:Long = idealized.n
  def P:Double = idealized.P

  override def toString: String = s"BinomialEstimate(n = $n, P = $P, min = ${interval.min}, MAX = ${interval.MAX}, μ = $μ, σ² = ${`σ²`}, σ = $σ, ℕ = $ℕ)"
}