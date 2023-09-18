package ai.dragonfly.math.stats.probability.distributions


import ai.dragonfly.math.{ProbDistDemo}
import ai.dragonfly.math.stats.*

object PERTDemo {
  val demo = ProbDistDemo("PERT", PERT(5.0, 6.0, 11.0), DenseHistogramOfContinuousDistribution(11, 5.0, 11.0))
}
