package ai.dragonfly.math.examples

import ai.dragonfly.math.stats.*
import probability.distributions.*
import stream.*


trait Demonstrable {
  def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder
  def name:String
}

trait ProbabilityDistributionDemonstration[DOMAIN] extends Demonstrable {
  val dist:ProbabilityDistribution[DOMAIN]
  val binCount: Int
  val min:DOMAIN
  val MAX: DOMAIN
  val sampleSize:Int
  def makeHistogram(): UnivariateHistogram[DOMAIN]
  override def demo(implicit sb: StringBuilder = new StringBuilder()): StringBuilder = {
    sb.append(s"\nDemonstrating $name:\n\tgenerating $sampleSize random variables from $dist\n")
    val histogram: UnivariateHistogram[DOMAIN] = makeHistogram()
    for (i <- 0 until sampleSize) {
      histogram(dist.random())
    }
    sb.append(s"\n$histogram\n")
  }
}

object ProbabilityDistributionDemonstration {
  def apply(name: String, dist: ContinuousProbabilityDistribution):ContinuousProbabilityDistributionDemonstration = {
    val sixSigma = Math.round(6*(dist.standardDeviation))
    val min:Double = if (dist.min > dist.mean - sixSigma) dist.min else dist.mean - sixSigma
    val MAX:Double = if (dist.MAX < dist.mean + sixSigma) dist.MAX else dist.mean + sixSigma

    val binCount:Int = Math.max(10, 4*(min - MAX).toInt)
    ContinuousProbabilityDistributionDemonstration(name, dist, binCount, min, MAX, 1000)
  }

  def apply(name: String, dist: DiscreteProbabilityDistribution):DiscreteProbabilityDistributionDemonstration = {
    val sixSigma = Math.round(6*(dist.standardDeviation))
    val min:Long = if (dist.min > dist.mean - sixSigma) dist.min else (dist.mean - sixSigma).toLong
    val MAX:Long = if (dist.MAX < dist.mean + sixSigma) dist.MAX else (dist.mean + sixSigma).toLong

    val binCount:Int = Math.max(10, 4*(min - MAX).toInt)
    DiscreteProbabilityDistributionDemonstration(name, dist, binCount, min, MAX, 1000)
  }
}

case class ContinuousProbabilityDistributionDemonstration(
  override val name: String,
  override val dist: ContinuousProbabilityDistribution,
  override val binCount: Int,
  override val min:Double,
  override val MAX: Double,
  override val sampleSize: Int = 1000
) extends ProbabilityDistributionDemonstration[Double] {
  override def makeHistogram(): UnivariateHistogram[Double] = new DenseHistogramOfContinuousDistribution(binCount, min, MAX)
}

case class DiscreteProbabilityDistributionDemonstration(
 override val name: String,
 override val dist: DiscreteProbabilityDistribution,
 override val binCount: Int,
 override val min:Long,
 override val MAX: Long,
 override val sampleSize: Int = 1000
) extends ProbabilityDistributionDemonstration[Long] {
  override def makeHistogram(): UnivariateHistogram[Long] = new DenseHistogramOfDiscreteDistribution(binCount, min, MAX)
}

trait OnlineProbDistDemo[DOMAIN] extends Demonstrable {
  val name: String
  val idealDist: ProbabilityDistribution[DOMAIN]
  val streamingDist: Online[DOMAIN]
  val sampleSize: Int

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


class ContinuousOnlineProbDistDemo(
  override val name: String,
  override val idealDist: ContinuousProbabilityDistribution,
  override val streamingDist: OnlineContinuous,
  override val sampleSize: Int
) extends OnlineProbDistDemo[Double]  {


}


class DiscreteOnlineProbDistDemo(
  override val name: String,
  override val idealDist: DiscreteProbabilityDistribution,
  override val streamingDist: OnlineDiscrete,
  override val sampleSize: Int
) extends OnlineProbDistDemo[Long]  {


}
