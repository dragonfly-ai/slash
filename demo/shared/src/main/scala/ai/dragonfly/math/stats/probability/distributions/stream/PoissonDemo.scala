package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.stats.probability.distributions
import ai.dragonfly.math.*

import scala.language.postfixOps
import scala.language.implicitConversions

object PoissonDemo {
  val demo = OnlineProbDistDemo[Long, distributions.Poisson, Poisson]("Streaming Poisson", distributions.Poisson(69), Poisson(), 10000)
}
