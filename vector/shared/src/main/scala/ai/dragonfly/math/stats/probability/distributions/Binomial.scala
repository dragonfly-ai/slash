package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.*
import ai.dragonfly.math.Factorial.!
import ai.dragonfly.math.examples.ProbabilityDistributionDemonstration

import scala.util.Random
import scala.language.postfixOps

object Binomial {
  val demo = ProbabilityDistributionDemonstration("Binomial", Binomial(21, 0.42))
}

/**
 * Binomial Distribution: https://en.wikipedia.org/wiki/Binomial_distribution
 *
 * @param n number of trials in process or experiment with two possible outcomes, i.e. a boolean valued random function.
 * @param `P(Success)` probability of a successful outcome.
 */
case class Binomial(n:Long, `P(Success)`:Double) extends DiscreteProbabilityDistribution {

  override val min: Long = 0
  override val MAX: Long = n

  /**
   * `1-p` is the probability of failure.
   */
  val `1-p`:Double = 1.0 - `P(Success)`

  override val μ: Double = n * `P(Success)`
  override val `σ²`: Double = n * `P(Success)` * `1-p`
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
      (`n!`/ (`k!` * (`n-k`!))).toDouble * Math.pow(`P(Success)`, k.toDouble) * Math.pow(1.0 - `P(Success)`, `n-k`.toDouble)
    }
  }

  inline def squaredDist(a:Double, b:Double):Double = {
    val amb = a - b
    amb*amb
  }

  /**
   * Naive implementation not suitable for large N.
   * @return a random number of successes.
   */
  override def random(): Long = {
    var successCount = 0L
    for (i <- 0L until n) {
      if (Math.random() < `P(Success)`) successCount = successCount + 1L
    }
    successCount
  }

}
