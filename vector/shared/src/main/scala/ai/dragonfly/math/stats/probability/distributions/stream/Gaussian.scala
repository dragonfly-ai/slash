package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.stats.*
import probability.distributions
import jdk.jfr.Frequency

import scala.util.Random


class Gaussian extends OnlineUnivariateProbabilityDistributionEstimator[Double, distributions.Gaussian]  {

  val estimator: PointStatisticsEstimator[Double] = new PointStatisticsEstimator[Double](distributions.Gaussian.domain)

  override def observe(frequency: Double, observation: Double): Gaussian = {
    estimator.observe(Array[Double](frequency, observation))
    this
  }

  override def estimate:distributions.EstimatedGaussian = {
    val sps:PointStatistics[Double] = estimator.samplePointStatistics
    distributions.EstimatedGaussian(
      sps.bounds,
      distributions.Gaussian(sps.μ, sps.`σ²`),
      sps.ℕ
    )
  }

}
