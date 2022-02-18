package ai.dragonfly.math

import ai.dragonfly.math.stats.DenseDiscreteHistogram
import ai.dragonfly.math.stats.probability.distributions.Beta
import org.apache.commons.math3.distribution.BetaDistribution

object Verify extends App {

  val parameters:Seq[Double] = Seq[Double](0.1, 0.5, 1.0, 2.0, 5.0)

  for (alpha <- parameters) {
    for (beta <- parameters) {
      val dragonfly: Beta = Beta(alpha, beta)
      val apache: BetaDistribution = BetaDistribution(alpha, beta)

      val dragonflyHist: DenseDiscreteHistogram = new DenseDiscreteHistogram(10, dragonfly.min, dragonfly.MAX)
      val apacheHist: DenseDiscreteHistogram = new DenseDiscreteHistogram(10, dragonfly.min, dragonfly.MAX)

      for (i <- 0 until 10000) {
        dragonflyHist(dragonfly.random())
        apacheHist(apache.sample())
      }

      println(s"Beta($alpha, $beta)\tapache\tdragonfly")
      println(compareHistograms(apacheHist, dragonflyHist))
    }
  }

  def compareHistograms(h1:DenseDiscreteHistogram, h2:DenseDiscreteHistogram): String = {
    val sb:StringBuilder = new StringBuilder()
    for (i <- 0 until h1.binCount) {
      sb.append(s"${h1.bucketLabel(i)}\t${h1.hist(i)}\t${h2.hist(i)}\n")
    }
    sb.toString()
  }

}
