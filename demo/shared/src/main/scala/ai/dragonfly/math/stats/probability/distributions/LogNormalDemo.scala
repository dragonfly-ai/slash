package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.*
import stats.*
import interval.*
import Constant.π

// https://en.wikipedia.org/wiki/Log-normal_distribution#Generation_and_parameters

object LogNormalDemo {

  val ln15_5:LogNormal = LogNormal(15.0, 5.0)
  val demo = ProbDistDemo("LogNormal", ln15_5, DenseHistogramOfContinuousDistribution(21, 1, ln15_5.μ + (6.0*ln15_5.σ)))

}
