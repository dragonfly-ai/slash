package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.stats.probability.distributions
import ai.dragonfly.math.*
import examples.*

import scala.language.postfixOps
import scala.language.implicitConversions

object Poisson {
  val demo = DiscreteOnlineProbDistDemo("Streaming Poisson", distributions.Poisson(69), Poisson(), 10000)
}

class Poisson extends OnlineDiscrete {
  private var minObservation = Long.MaxValue
  private var maxObservation = Long.MinValue

  private var s0:Long = 0L // weighted count
  private var s1:Long = 0L // weighted sum

  /**
   * Assumes only positive valued observations.
   * @param observation the value observed.
   * @param frequency the number of times this value has been observed, default 1L
   */
  override def apply(observation: Long, frequency: Long):Poisson = if (observation < 0) {
    throw PoissonDistributionUndefinedForNegativeNumbers(observation)
  } else {
    minObservation = Math.min(observation, minObservation)
    maxObservation = Math.max(observation, maxObservation)

    s0 = s0 + frequency
    s1 = s1 + observation * frequency

    this
  }

  override def min:Long = minObservation
  override def MAX:Long = maxObservation

  def n:Long = s0

  inline def λ:Double = (s1 / s0).toDouble

  def μ:Double = λ

  inline def `σ²`: Double = λ //  s1 / s0

  def σ:Double = Math.sqrt(λ) // Math.sqrt(`σ²`)

  def freeze:distributions.Poisson = distributions.Poisson(λ)

  def p(x:Long):Double = Math.exp( x * Math.log(λ) - λ - Math.log(gamma(x.toDouble+1.0)) )

  override def random(): Long = freeze.random()

  override def toString: String = s"stream.Poisson(min = $min, MAX = $MAX, λ = μ = σ² = $λ, σ = √λ = $σ, N = $s0)"

}

case class PoissonDistributionUndefinedForNegativeNumbers(negative:Long) extends Exception(s"Poisson distribution undefined for observation: $negative")