package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.*
import stats.*

object PoissonDemo {
  val p15:Poisson = Poisson(15)
  val σ6:Long = Math.ceil(p15.σ * 6).toLong
  val demo = ProbDistDemo("Poisson", p15, DenseHistogramOfDiscreteDistribution(15, p15.λ.toLong - σ6, p15.λ.toLong + σ6))
}