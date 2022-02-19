package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.stats.probability.distributions.*

trait Online[DOMAIN] extends ProbabilityDistribution[DOMAIN]  {
  // ℕ population size symbol for future reference.
  def n:DOMAIN
  def sampleSize:DOMAIN = n
  def freeze:ProbabilityDistribution[DOMAIN]
  def apply(observation: DOMAIN): Online[DOMAIN]
  def apply(observation: DOMAIN, frequency: DOMAIN): Online[DOMAIN]
}

trait OnlineContinuous extends Online[Double] {
  override def apply(observation: Double):Online[Double] = apply(observation, 1.0)

}

trait OnlineDiscrete extends Online[Long] {
  def apply(observation: Long): Online[Long] = apply(observation, 1L)
}