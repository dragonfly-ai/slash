package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.stats.*
import probability.distributions
import ai.dragonfly.math.example.*

object Beta {
  val demo = OnlineProbDistDemo[Double, distributions.Beta, Beta]("Streaming Beta", distributions.Beta(3.0, 0.75, 42.0, 69.0), Beta(), 1000000)
}

class Beta extends OnlineUnivariateProbabilityDistributionEstimator[Double, distributions.Beta]  {

  val estimator: PointStatisticsEstimator[Double] = new PointStatisticsEstimator[Double](distributions.Beta.domain)

  override def observe(frequency: Double, observation: Double): Beta = {
    estimator.observe( Array[Double](frequency, observation) )
    this
  }

  override def estimate:distributions.EstimatedBeta = {
    val sps:PointStatistics[Double] = estimator.samplePointStatistics
    distributions.EstimatedBeta(
      distributions.Beta.fromMeanVarianceMinMax(sps.μ, sps.`σ²`, sps.min, sps.MAX),
      sps.ℕ̂
    )
  }

}