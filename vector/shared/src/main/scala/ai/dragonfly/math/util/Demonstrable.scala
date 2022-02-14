package ai.dragonfly.math.util

trait Demonstrable {
  def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder
  def name:String
}


import ai.dragonfly.math.stats.{DenseDiscreteHistogram, ProbabilityDistribution}
import ai.dragonfly.math.stats.stream.Online

object ProbDistDemo {
  def apply(name: String, dist: ProbabilityDistribution):ProbDistDemo = {
    val sixSigma = Math.round(6*(dist.standardDeviation))
    val min:Double = if (dist.min > dist.mean - sixSigma) dist.min else dist.mean - sixSigma
    val max:Double = if (dist.MAX < dist.mean + sixSigma) dist.MAX else dist.mean + sixSigma

    val size = Math.max(10, 4*(min - max).toInt)
    ProbDistDemo(name, dist, size, min, max, 1000)
  }
}

case class ProbDistDemo(override val name: String, dist: ProbabilityDistribution, size: Int, min:Double, max: Double, sampleSize: Int = 1000) extends Demonstrable {

  override def demo(implicit sb: StringBuilder = new StringBuilder()): StringBuilder = {
    sb.append(s"\nDemonstrating $name:\n\tgenerating $sampleSize random variables from $dist\n")
    val histogram: DenseDiscreteHistogram = new DenseDiscreteHistogram(size, min, max)
    for (i <- 0 until sampleSize) {
      histogram(dist.random())
    }
    sb.append(s"\n$histogram\n")
  }
}

class OnlineProbDistDemo[PD <: ProbabilityDistribution](override val name: String, idealDist: PD, streamingDist: Online[PD], sampleSize: Int) extends Demonstrable {

  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    sb.append(s"Estimate $name by sampling from: $idealDist")
    val blockSize:Int = sampleSize / 5
    streamingDist(idealDist.random())
    for (i <- 1 until sampleSize) {
      streamingDist(idealDist.random())
      if (i % blockSize == 0) {
        sb.append(s"\n\tIteration $i estimated: ${streamingDist.freeze} ideal: $idealDist")
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
