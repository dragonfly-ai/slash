package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.OnlineProbDistDemo
import ai.dragonfly.math.stats.*
import probability.distributions
import jdk.jfr.Frequency

import scala.util.Random

object GaussianDemo {
  val demo = OnlineProbDistDemo[Double, distributions.Gaussian, Gaussian]("Streaming Gaussian", distributions.Gaussian(42.0, 7.0), Gaussian(), 1000)
}
