package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.*
import stats.*
import ai.dragonfly.math.interval.*
import Interval.*

import scala.language.postfixOps

object DiscreteUniformDemo {
  val demo = ProbDistDemo(
    "Discrete Uniform",
    DiscreteUniform(5, 15),
    DenseHistogramOfDiscreteDistribution(7, 5, 15)
  )
}
