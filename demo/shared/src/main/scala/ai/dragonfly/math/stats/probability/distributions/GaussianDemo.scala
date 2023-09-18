package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.*
import stats.*



object GaussianDemo {
  val g10_42:Gaussian = Gaussian(10.0, 42.0)
  val σ6:Long = Math.ceil(g10_42.σ * 6).toLong
  val demo = ProbDistDemo( "Gaussian", g10_42, DenseHistogramOfContinuousDistribution(13, g10_42.μ - σ6, g10_42.μ + σ6 ))
}
