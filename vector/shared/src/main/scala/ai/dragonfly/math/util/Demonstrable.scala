package ai.dragonfly.math.util

trait Demonstrable {
  def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder
  def name:String
}


import ai.dragonfly.math.stats.{DenseDiscreteHistogram, ProbabilityDistribution}
import ai.dragonfly.math.stats.stream.Online

class ProbDistDemo(override val name: String, dist: ProbabilityDistribution, sampleSize: Int = 1000) extends Demonstrable {

  override def demo(implicit sb: StringBuilder = new StringBuilder()): StringBuilder = {
    sb.append(s"\nDemonstrating $name:\n\tgenerating $sampleSize random variables from $dist\n")
    val sixSigma = Math.round(6*(dist.standardDeviation))
    val histogram: DenseDiscreteHistogram = new DenseDiscreteHistogram(36, dist.mean - sixSigma, dist.mean + sixSigma)
    for (i <- 0 until sampleSize) {
      histogram(dist.random())
    }
    sb.append(s"\n\t$histogram")
  }
}

class OnlineProbDistDemo[PD <: ProbabilityDistribution](override val name: String, idealDist: PD, streamingDist: Online[PD], sampleSize: Int) extends Demonstrable {

  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    sb.append(s"Estimate $name by sampling from: $idealDist")
    for (i <- 0 until 10000) {
      streamingDist(idealDist.random())
      if (i % 2500 == 3) {
        sb.append(s"\n\tFreeze estimated distribution and compare to ideal: ${streamingDist.freeze} vs. $idealDist")
      }
    }
    sb.append(s"\n\tEstimated Distribution: $streamingDist\n\tFrom Distribution: $idealDist\n")
    sb.append(s"\nTest $idealDist.p($idealDist.random())")
    for (i <- 0 until 5) {
      val x = idealDist.random()
      sb.append(s"\n\tp($x) = ${idealDist.p(x)}")
    }
    sb.append("\n")
  }

}
