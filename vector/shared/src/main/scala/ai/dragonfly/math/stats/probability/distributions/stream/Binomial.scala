package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.stats.probability.distributions
import ai.dragonfly.math.*
import ai.dragonfly.math.interval.*
import Interval.*
import ai.dragonfly.math.stats.BoundedMean
import example.*

import scala.language.postfixOps
import scala.language.implicitConversions

object Binomial {

  val fixedBinomialDemo = OnlineProbDistDemo[Long, distributions.Binomial, FixedBinomial]("Streaming FixedBinomial", distributions.Binomial(42L, 0.69), FixedBinomial(42L), 1000)


  val openBinomialDemo = new Demonstrable {
    override def name: String = "Streaming Binomial"

    def si:Double = 1 + ((Math.random() - 0.5) / 8.0)

    override def demo(implicit sb: StringBuilder): StringBuilder = {
      val sampleSize = 1000
      val idealDist = distributions.Binomial(69, 0.42)
      val streamingDist = Binomial()
      sb.append(s"Estimate $name:\n\tSampling: $idealDist")
      val blockSize:Int = sampleSize / 5
      val end = sampleSize + 1
      for (i <- 1 until end) {
        val ki = idealDist.random() * si
        val ni = idealDist.n * si
        streamingDist.observe(ki.toLong, ni.toLong)
        if (i % blockSize == 0) {
          sb.append(s"\n\t\testimation after $i samples: ${streamingDist.estimate}")
        }
      }
      sb.append(s"\n\tEstimate: ${streamingDist.estimate}\n\tIdeal Distribution: $idealDist\n")
      sb.append(s"\nTest $idealDist.p($idealDist.random())")

      for (i <- 0 until 5) {
        val x = idealDist.random()
        sb.append(s"\n\tp($x) = ${idealDist.p(x)}")
      }
      sb.append("\n")
    }

  }
}


case class FixedBinomial(n: Long) extends OnlineUnivariateProbabilityDistributionEstimator[Long, distributions.Binomial]  {

  val estimator: BoundedMeanEstimator[Long] = new BoundedMeanEstimator[Long](distributions.Binomial.domain)

  override def observe(frequency: Long, observation: Long): FixedBinomial = {
    estimator.observe(Array[Long](frequency, observation))
    this
  }

  override def estimate:distributions.EstimatedBinomial = {
    val bμ̂:BoundedMean[Long] = estimator.sampleBoundedMean
    distributions.EstimatedBinomial(
      bμ̂.bounds,
      distributions.Binomial(n, bμ̂.μ / n),
      bμ̂.ℕ
    )
  }

}



class Binomial extends OnlineBivariateProbabilityDistributionEstimator[Long, distributions.Binomial] {

  val estimator = new BinomialEstimator()

  def observe(experimentCount:Long, successCount:Long, trialCount:Long): Binomial = {
    estimator.observe(Array[Long](experimentCount, successCount, trialCount))
    this
  }

  def estimate:distributions.EstimatedBinomial = {
    val si = estimator.getS
    distributions.EstimatedBinomial(
      `[]`[Long](si(3), si(4)),
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
