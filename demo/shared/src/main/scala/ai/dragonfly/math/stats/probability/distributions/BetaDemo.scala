package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.*
import stats.*
import interval.*
import Interval.*

object BetaDemo {
  val demo2param = ProbDistDemo("Beta", Beta(0.5, 5.0), DenseHistogramOfContinuousDistribution(9, 0, 1))
  val demo4param = ProbDistDemo("Beta", Beta(2.0, 1.0, 33, 42), DenseHistogramOfContinuousDistribution(15, 33, 42))
}
