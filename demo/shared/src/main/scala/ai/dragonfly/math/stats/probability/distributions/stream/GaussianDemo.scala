package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.OnlineProbDistDemo
import ai.dragonfly.math.stats.*
import probability.distributions

object GaussianDemo {
  val demo: OnlineProbDistDemo[Double, distributions.Gaussian, Gaussian] = OnlineProbDistDemo[Double, distributions.Gaussian, Gaussian]("Streaming Gaussian", distributions.Gaussian(42.0, 7.0), Gaussian(), 1000)
}
