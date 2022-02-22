package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.*
import stats.*
import interval.*
import probability.distributions.*

import scala.reflect.ClassTag

trait OnlineEstimator[DOMAIN](using `#`: Numeric[DOMAIN] , tag: ClassTag[DOMAIN]) {

  val domain:Domain[DOMAIN]

  def observe(xi:Array[DOMAIN]):OnlineEstimator[DOMAIN]
  protected var s:Array[DOMAIN] = null
}

trait OnlineProbabilityDistributionEstimator[DOMAIN, PPD <: ParametricProbabilityDistribution[DOMAIN]](using `#`: Numeric[DOMAIN] , tag: ClassTag[DOMAIN]) {
  def apply(p1:DOMAIN, p2:DOMAIN):OnlineProbabilityDistributionEstimator[DOMAIN, PPD]
  def estimate:EstimatedProbabilityDistribution[DOMAIN, PPD]
}

trait EstimatesRange[DOMAIN] {
  def sampleRange:Interval[DOMAIN]
}

class RangeEstimator[DOMAIN](override val domain:Domain[DOMAIN])(using `#`: Numeric[DOMAIN] , tag: ClassTag[DOMAIN]) extends OnlineEstimator[DOMAIN] with EstimatesRange[DOMAIN] {

  // initialize
  s = Array[DOMAIN](
    domain.MAX,  // initialize min
    domain.min   // initialize MAX
  )


  override def observe(xi:Array[DOMAIN]):RangeEstimator[DOMAIN] = {
    s = Array[DOMAIN](
      `#`.min(xi(0), s(0)), // min
      `#`.max(xi(0), s(1)), // MAX
    )
    this
  }

  override def sampleRange:Interval[DOMAIN] = {
    val si = s
    `[]`(si(0), si(1))
  }

}

trait EstimatesMean {
  def sampleMean:Double
}

class MeanEstimator[DOMAIN](override val domain:Domain[DOMAIN])(using `#`: Numeric[DOMAIN] , tag: ClassTag[DOMAIN]) extends OnlineEstimator[DOMAIN] with EstimatesMean {

  s = scala.Array[DOMAIN](`#`.zero, `#`.zero)

  override def observe(xi:Array[DOMAIN]):MeanEstimator[DOMAIN] = synchronized {
    import `#`._;
    s = Array[DOMAIN](
      s(0) + xi(0),  // sample size
      s(1) + xi(0) * xi(1)  // sample sum
    )
    this
  }

  override def sampleMean:Double = {
    import `#`._;
    val si = s
    si(1).toDouble / si(0).toDouble
  }

}
trait EstimatesBoundedMean[DOMAIN] extends EstimatesMean with EstimatesRange[DOMAIN] {
  def sampleBoundedMean:BoundedMean[DOMAIN]
}

class BoundedMeanEstimator[DOMAIN](override val domain:Domain[DOMAIN])(using `#`: Numeric[DOMAIN] , tag: ClassTag[DOMAIN]) extends OnlineEstimator[DOMAIN] with EstimatesBoundedMean[DOMAIN] {
  s = scala.Array[DOMAIN](
    `#`.zero, // sample size
    `#`.zero,  // sample sum
    domain.MAX,  // initialize min
    domain.min   // initialize MAX
  )

  override def observe(xi:Array[DOMAIN]): BoundedMeanEstimator[DOMAIN] = synchronized {
    import `#`._
    s = Array[DOMAIN](
      s(0) + xi(0),  // sample size
      s(1) + (xi(1) * xi(0)),  // sample sum
      min(xi(1), s(2)), // min
      max(xi(1), s(3)), // MAX
    )
    this
  }

  override def sampleRange:Interval[DOMAIN] = sampleBoundedMean.bounds

  override def sampleMean:Double = sampleBoundedMean.μ

  override def sampleBoundedMean:BoundedMean[DOMAIN] = {
    import `#`._;
    val si = s
    BoundedMean[DOMAIN](
      si(1).toDouble / si(0).toDouble,
      `[]`(si(2), si(3)),
      si(0)
    )
  }
}

trait EstimatesMeanAndVariance[DOMAIN] extends EstimatesMean {
  def sampleVariance:Double
  def sampleMeanAndVariance:MeanAndVariance[DOMAIN]
}

class MeanAndVarianceEstimator[DOMAIN](override val domain:Domain[DOMAIN])(using `#`: Numeric[DOMAIN] , tag: ClassTag[DOMAIN]) extends OnlineEstimator[DOMAIN] with EstimatesMeanAndVariance[DOMAIN] {
  s = Array[DOMAIN](`#`.zero, `#`.zero, `#`.zero)

  override def observe(xi:Array[DOMAIN]): MeanAndVarianceEstimator[DOMAIN] = synchronized {
    import `#`._;
    s = Array[DOMAIN](
      s(0) + xi(0),  // sample size
      s(1) + (xi(1) * xi(0)),  // sample sum
      s(2) + ((xi(1) * xi(1)) * xi(0)) // sum of weighted samples squared
    )
    this
  }

  override def sampleMean:Double = sampleMeanAndVariance.μ

  override def sampleVariance:Double = sampleMeanAndVariance.`σ²`

  override def sampleMeanAndVariance:MeanAndVariance[DOMAIN] = {
    import `#`._;
    val si = s
    MeanAndVariance[DOMAIN](
      si(1).toDouble / si(0).toDouble,
      (si(0) * si(2) - si(1) * si(1)).toDouble / (si(0) * (si(0) - `#`.one)).toDouble,
      si(0)
    )
  }
}
trait EstimatesPointStatistics[DOMAIN] extends EstimatesMeanAndVariance[DOMAIN] with EstimatesBoundedMean[DOMAIN] {
  def samplePointStatistics:PointStatistics[DOMAIN]
}

class PointStatisticsEstimator[DOMAIN](override val domain:Domain[DOMAIN])(using `#`: Numeric[DOMAIN], tag: ClassTag[DOMAIN]) extends OnlineEstimator[DOMAIN] with EstimatesPointStatistics[DOMAIN] {

  s = Array[DOMAIN](
    `#`.zero, `#`.zero, `#`.zero,
    domain.MAX,  // initialize min
    domain.min   // initialize MAX
  )

  override def observe(xi:Array[DOMAIN]): PointStatisticsEstimator[DOMAIN] = synchronized {
    import `#`._
    s = Array[DOMAIN](
      s(0) + xi(0),  // sample size
      s(1) + (xi(1) * xi(0)),  // sample sum
      s(2) + ((xi(1) * xi(1)) * xi(0)), // sum of weighted samples squared
      min(xi(1), s(3)), // min
      max(xi(1), s(4)), // MAX
    )
    this
  }

  override def sampleRange:Interval[DOMAIN] = samplePointStatistics.bounds

  override def sampleMean:Double = samplePointStatistics.μ

  override def sampleVariance:Double = samplePointStatistics.`σ²`

  override def sampleBoundedMean:BoundedMean[DOMAIN] = {
    val sps = samplePointStatistics
    BoundedMean[DOMAIN](sps.μ, sps.bounds, sps.ℕ̂)
  }

  override def sampleMeanAndVariance:MeanAndVariance[DOMAIN] = {
    val sps = samplePointStatistics
    MeanAndVariance[DOMAIN](sps.μ, sps.`σ²`, sps.ℕ̂)
  }

  override def samplePointStatistics:PointStatistics[DOMAIN] = {
    import `#`._;
    val si = s
    PointStatistics[DOMAIN](
      si(1).toDouble / si(0).toDouble,
      (si(0) * si(2) - si(1) * si(1)).toDouble / (si(0) * (si(0) - `#`.one)).toDouble,
      `[]`(si(3), si(4)),
      si(0)
    )
  }
}