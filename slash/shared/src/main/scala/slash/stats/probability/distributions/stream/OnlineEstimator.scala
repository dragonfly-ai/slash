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

package slash.stats.probability.distributions.stream

import slash.*
import stats.*
import slash.interval.*
import probability.distributions.*

import scala.reflect.ClassTag

trait OnlineEstimator {
  def sampleMass:BigDecimal
}

trait OnlineUnivariateEstimator[DOMAIN:ClassTag] extends OnlineEstimator {
  def observe(observation:DOMAIN):this.type
  def observe(frequency:DOMAIN, observation:DOMAIN):this.type
}

trait OnlineBivariateEstimator[DOMAIN:ClassTag] extends OnlineEstimator {
  def observe(observation1:DOMAIN, observation2:DOMAIN):this.type
  def observe(frequency:DOMAIN, observation1:DOMAIN, observation2:DOMAIN):this.type
}

trait OnlineProbabilityDistributionEstimator[DOMAIN:ClassTag, PPD <: ParametricProbabilityDistribution[DOMAIN]] {
  def estimate:EstimatedProbabilityDistribution[DOMAIN, PPD]
}

trait EstimatesRange[DOMAIN:ClassTag] extends OnlineUnivariateEstimator[DOMAIN] {
  def sampleRange:Interval[DOMAIN]
}

trait EstimatesMean[DOMAIN:ClassTag] extends OnlineUnivariateEstimator[DOMAIN] {
  def sampleMean:Double
}

trait EstimatesBoundedMean[DOMAIN:ClassTag] extends EstimatesMean[DOMAIN] with EstimatesRange[DOMAIN] {
  def sampleBoundedMean:SampleBoundedMean[DOMAIN] = SampleBoundedMean[DOMAIN]( sampleMean, sampleRange, sampleMass )
}

trait EstimatesMeanAndVariance[DOMAIN:ClassTag] extends EstimatesMean[DOMAIN] {
  def sampleVariance:Double
  def sampleMeanAndVariance:SampleMeanAndVariance = SampleMeanAndVariance(sampleMean, sampleVariance, sampleMass)
}

trait EstimatesPointStatistics[DOMAIN:ClassTag] extends EstimatesMeanAndVariance[DOMAIN] with EstimatesRange[DOMAIN] {
  def samplePointStatistics:SamplePointStatistics[DOMAIN] = SamplePointStatistics[DOMAIN](
    sampleMean,
    sampleVariance,
    sampleRange,
    sampleMass
  )

}