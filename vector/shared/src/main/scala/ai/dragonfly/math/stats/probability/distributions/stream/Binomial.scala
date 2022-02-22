package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.stats.probability.distributions
import ai.dragonfly.math.*
import ai.dragonfly.math.interval.Domain
import examples.*

import scala.language.postfixOps
import scala.language.implicitConversions

object Binomial {
  val demo = new Demonstrable {
    override def name: String = "Streaming Binomial"

    override def demo(implicit sb: StringBuilder): StringBuilder = {
      val sampleSize = 1000
      val idealDist = distributions.Binomial(69, 0.42)
      val streamingDist = Binomial()
      sb.append(s"Estimate ${this.name} by sampling from: $idealDist")
      val blockSize:Int = sampleSize / 5
      streamingDist(idealDist.random(), 69)
      for (i <- 1 until sampleSize) {
        streamingDist(idealDist.random(), 69)
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
}



class Binomial extends OnlineProbabilityDistributionEstimator[Long, distributions.Binomial] {

  val estimator = new BinomialEstimator()

  def apply(successCount:Long, trialCount:Long): Binomial = apply(1L, successCount, trialCount)

  def apply(experimentCount:Long, successCount:Long, trialCount:Long): Binomial = {
    estimator.observe(Array[Long](experimentCount, successCount, trialCount))
    this
  }

  def estimate:distributions.EstimatedBinomial = {
    val si = estimator.getS
    distributions.EstimatedBinomial(
      interval.`[]`[Long](si(3), si(4)),
      distributions.Binomial(
        si(2) / si(0), // estimated trial count per experiment
        ( BigDecimal(si(1)) / BigDecimal(si(2)) ).toDouble  // estimated Probability of success per trial.
      ),
      si(0)
    )
  }
}

class BinomialEstimator(override val domain:Domain[Long] = distributions.Binomial.domain) extends OnlineEstimator[Long] {

  s = Array[Long](0L, 0L, 0L, Long.MaxValue, Long.MinValue)

  override def observe(xi:Array[Long]):OnlineEstimator[Long] = synchronized {
    val si = s
    s = Array[Long](
      si(0) + xi(0),  // experiment count
      si(1) + xi(1) * xi(0),  // success count
      si(2) + xi(2) * xi(0),   // trial count
      Math.min(si(3), xi(1)),
      Math.max(si(4), xi(1))
    )
    this
  }

  def getS:Array[Long] = s

}
