package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.*
import ai.dragonfly.math.stats.probability.distributions


object LogNormalDemo {
  val demo = OnlineProbDistDemo[Double, distributions.LogNormal, LogNormal]("Streaming LogNormal", distributions.LogNormal(69, 21), LogNormal(), 1000)
}