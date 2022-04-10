package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.stats.probability.distributions
import ai.dragonfly.math.*
import example.*

import scala.language.postfixOps
import scala.language.implicitConversions

object Poisson {
  val demo = OnlineProbDistDemo[Long, distributions.Poisson, Poisson]("Streaming Poisson", distributions.Poisson(69), Poisson(), 10000)
}

class Poisson extends OnlineUnivariateProbabilityDistributionEstimator[Long, distributions.Poisson] {

  val estimator = new BoundedMeanEstimator[Long](distributions.Poisson.domain)

  override def observe(frequency: Long, observation: Long):Poisson = {
    estimator.observe(Array[Long](frequency, observation))
    this
  }

  override def estimate:distributions.EstimatedPousson = {
    val bμ = estimator.sampleBoundedMean
    distributions.EstimatedPousson(
      bμ.bounds,
      distributions.Poisson(bμ.μ),
      bμ.ℕ
    )
  }

}

case class PoissonDistributionUndefinedForNegativeNumbers(negative:Long) extends Exception(s"Poisson distribution undefined for observation: $negative")