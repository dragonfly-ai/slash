package ai.dragonfly.math.examples

import ai.dragonfly.math.stats.*
import probability.distributions.*
import stream.*


trait Demonstrable {
  def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder
  def name:String
}

case class ProbabilityDistributionDemonstration[DOMAIN] (
  name:String,
  dist:ParametricProbabilityDistribution[DOMAIN],
  val histogram: UnivariateHistogram[DOMAIN],
  sampleSize:Int = 10000
) extends Demonstrable {
  override def demo(implicit sb: StringBuilder = new StringBuilder()): StringBuilder = {
    sb.append(s"\nDemonstrating $name:\n\tgenerating $sampleSize random variables from $dist\n")
    for (i <- 0 until sampleSize) {
      histogram(dist.random())
    }
    sb.append(s"\n$histogram\n")
  }
}



case class OnlineProbDistDemo[DOMAIN, PPD <: ParametricProbabilityDistribution[DOMAIN], EPPD <: OnlineProbabilityDistributionEstimator[DOMAIN, PPD]](
  name: String,
  idealDist: PPD,
  streamingDist: EPPD,
  sampleSize: Int
)(using `#`: Numeric[DOMAIN]) extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    sb.append(s"Estimate $name by sampling from: $idealDist")
    val blockSize:Int = sampleSize / 5
    streamingDist(`#`.one, idealDist.random())
    for (i <- 1 until sampleSize) {
      streamingDist(`#`.one, idealDist.random())
      if (i % blockSize == 0) {
        sb.append(s"\n\tIteration $i estimated: ${streamingDist.estimate} ideal: $idealDist")
      }
    }
    sb.append(s"\n\tEstimated Distribution: ${streamingDist.estimate}\n\tFrom Distribution: $idealDist\n")
    sb.append(s"\nTest $idealDist.p($idealDist.random())")
    for (i <- 0 until 5) {
      val x = idealDist.random()
      sb.append(s"\n\tp($x) = ${idealDist.p(x)}")
    }
    sb.append("\n")
  }
}
