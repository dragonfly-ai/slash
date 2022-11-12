package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.*

import stats.*
import ai.dragonfly.math.interval.*
import Interval.*


import scala.language.postfixOps

object UniformDemo {
  val demo = ProbDistDemo("Uniform", Uniform(5.0, 11.0), DenseHistogramOfContinuousDistribution(7, 5, 11))
}
