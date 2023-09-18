package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.*
import stats.*
import ai.dragonfly.math.interval.*


import scala.language.postfixOps

object BinomialDemo {
  val demo = ProbDistDemo("Binomial", Binomial(21, 0.42), DenseHistogramOfDiscreteDistribution(11, 0, 21))
  lazy val domain:Domain[Long] = Domain.ℕ_Long
}
