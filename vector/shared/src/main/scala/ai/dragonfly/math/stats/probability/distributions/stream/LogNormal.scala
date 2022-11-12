package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.interval.*
import Interval.*
import ai.dragonfly.math.stats.probability.distributions
import ai.dragonfly.math.stats.PointStatistics


class LogNormal extends OnlineUnivariateProbabilityDistributionEstimator[Double, distributions.LogNormal]  {

  val estimator = new PointStatisticsEstimator[Double](distributions.LogNormal.domain)

  override def observe(frequency: Double, observation: Double):LogNormal = {
    estimator.observe(Array[Double](frequency, Math.log(observation)))
    this
  }

  override def estimate:distributions.EstimatedLogNormal = {
    val sps:PointStatistics[Double] = estimator.samplePointStatistics

    distributions.EstimatedLogNormal(
      `[]`[Double](Math.exp(sps.min), Math.exp(sps.MAX)),
      distributions.LogNormal.fromGaussianParameters(sps.μ, sps.`σ²`),
      sps.ℕ
    )
  }
}
