/*
 * Copyright 2023 dragonfly.ai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.*
import stats.*
import ai.dragonfly.math.interval.*
import probability.distributions.*

import scala.reflect.ClassTag

trait OnlineEstimator[DOMAIN:ClassTag] {
  def totalSampleMass:DOMAIN
}

trait OnlineUnivariateEstimator[DOMAIN:ClassTag] extends OnlineEstimator[DOMAIN] {
  def observe(observation:DOMAIN):this.type
  def observe(frequency:DOMAIN, observation:DOMAIN):this.type
}

trait OnlineBivariateEstimator[DOMAIN:ClassTag] extends OnlineEstimator[DOMAIN] {
  def observe(observation1:DOMAIN, observation2:DOMAIN):this.type
  def observe(frequency:DOMAIN, observation1:DOMAIN, observation2:DOMAIN):this.type
}

trait OnlineProbabilityDistributionEstimator[DOMAIN:ClassTag, PPD <: ParametricProbabilityDistribution[DOMAIN]] {
  def estimate:EstimatedProbabilityDistribution[DOMAIN, PPD]
}

trait EstimatesRange[DOMAIN:ClassTag] extends OnlineUnivariateEstimator[DOMAIN] {
//  protected var min:DOMAIN
//  protected var MAX:DOMAIN
  def estimatedRange:Interval[DOMAIN]
}

trait EstimatesMean[DOMAIN:ClassTag] extends OnlineUnivariateEstimator[DOMAIN] {

//  protected var s0: DOMAIN
//  protected var s1: DOMAIN

//  = {
//    s0 = s0 + frequency
//    s1 = s1 + observation * frequency
//    this
//  }

  def estimatedMean:Double
  //= s1 / s0

}

trait EstimatesBoundedMean[DOMAIN:ClassTag] extends EstimatesMean[DOMAIN] with EstimatesRange[DOMAIN] {

//  = {
//    s0 = s0 + frequency
//    s1 = s1 + observation * frequency
//    min = minOf[DOMAIN](min, observation)
//    MAX = MAXof[DOMAIN](MAX, observation)
//    this
//  }

  def estimatedBoundedMean:BoundedMean[DOMAIN] = BoundedMean[DOMAIN]( estimatedMean, estimatedRange, totalSampleMass )
}

trait EstimatesMeanAndVariance[DOMAIN:ClassTag] extends EstimatesMean[DOMAIN] {

//  protected var s2: DOMAIN

//  = {
//    s0 = s0 + frequency  // sample size
//    s1 = s1 + observation * frequency  // sample sum
//    s2 = s2 + (observation * observation) * frequency // sum of weighted samples squared
//    this
//  }

  def estimatedVariance:Double

  def estimatedMeanAndVariance:MeanAndVariance[DOMAIN] = MeanAndVariance[DOMAIN](estimatedMean, estimatedVariance, totalSampleMass)
//  = {
//    MeanAndVariance[DOMAIN](
//      s1 / s0,
//      (s0 * s2 - s1 * s1) / (s0 * (s0 - identities.one)),
//      s0
//    )
//  }
}

trait EstimatesPointStatistics[DOMAIN:ClassTag] extends EstimatesMeanAndVariance[DOMAIN] with EstimatesRange[DOMAIN] {

  def estimatedPointStatistics:PointStatistics[DOMAIN] = PointStatistics[DOMAIN](
    estimatedMean,
    estimatedVariance,
    estimatedRange,
    totalSampleMass
  )
//  = {
//    val mv = estimatedMeanAndVariance
//    PointStatistics[DOMAIN](
//      mv.μ,
//      mv.`σ²`,
//      estimagedRange,
//      s0
//    )
//    PointStatistics[DOMAIN](
//      s1 / s0,
//      (s0 * s2 - s1 * s1) / (s0 * (s0 - identities.one)),
//      `[]`[DOMAIN](min, MAX),
//      s0
//    )
//  }
}