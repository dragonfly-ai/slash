package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.stats.probability.distributions.ProbabilityDistribution

trait Online[PD <: ProbabilityDistribution] extends ProbabilityDistribution  {
  def freeze:PD
  def apply(observation: Double):Online[PD] = apply(observation, 1.0)
  def apply(observation: Double, frequency: Double):Online[PD]
}
