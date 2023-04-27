package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.*
import ai.dragonfly.math.stats.*
import probability.distributions

object BetaDemo {
  val demo = OnlineProbDistDemo[Double, distributions.Beta, Beta]("Streaming Beta", distributions.Beta(3.0, 0.75, 42.0, 69.0), Beta(), 10000)
}
