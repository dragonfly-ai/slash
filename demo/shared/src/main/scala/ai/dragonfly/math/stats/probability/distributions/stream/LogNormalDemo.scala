package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.*
import ai.dragonfly.math.interval.*
import Interval.*
import ai.dragonfly.math.stats.probability.distributions
import ai.dragonfly.math.stats.PointStatistics


object LogNormalDemo {
  val demo = OnlineProbDistDemo[Double, distributions.LogNormal, LogNormal]("Streaming LogNormal", distributions.LogNormal(69, 21), LogNormal(), 1000)
}