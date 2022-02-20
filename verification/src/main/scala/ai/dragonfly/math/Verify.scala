package ai.dragonfly.math

import ai.dragonfly.math.stats.DenseHistogramOfContinuousDistribution
import ai.dragonfly.math.stats.probability.distributions.Beta
import org.apache.commons.math3.distribution.BetaDistribution

object Verify extends App {

  val parameters:Seq[Double] = Seq[Double](0.1, 0.5, 1.0, 2.0, 5.0)

  for (alpha <- parameters) {
    for (beta <- parameters) {
      val dragonfly: Beta = Beta(alpha, beta)
      val apache: BetaDistribution = BetaDistribution(alpha, beta)

      val dragonflyHist: DenseHistogramOfContinuousDistribution = new DenseHistogramOfContinuousDistribution(10, dragonfly.min, dragonfly.MAX)
      val apacheHist: DenseHistogramOfContinuousDistribution = new DenseHistogramOfContinuousDistribution(10, dragonfly.min, dragonfly.MAX)

      for (i <- 0 until 10000) {
        dragonflyHist(dragonfly.random())
        apacheHist(apache.sample())
      }

      println(s"\n\nBeta($alpha, $beta)\tapache\tdragonfly")
      println(s"Apache $apacheHist\ndragonfly $dragonflyHist")
    }
  }


}
