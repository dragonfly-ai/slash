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

import slash.stats.probability.distributions
import slash.*
import slash.interval.*
import slash.accumulation.DiscreteAccumulator
import slash.interval.Interval

import scala.language.postfixOps
import scala.language.implicitConversions

case class FixedBinomial(trialCount: Long) extends OnlineProbabilityDistributionEstimator[Long, distributions.Binomial] with EstimatesBoundedMean[Long] {

  private var s0: Long = 0L
  private val s1: DiscreteAccumulator = new DiscreteAccumulator

  private var min: Long = Long.MaxValue
  private var MAX: Long = Long.MinValue

  override def observe(successCount: Long): this.type = observe(1L, successCount)

  override def observe(experimentCount: Long, successCount: Long): this.type = {
    s0 += experimentCount
    s1.observeProduct(successCount, experimentCount)

    min = Math.min(min, successCount)
    MAX = Math.min(MAX, successCount)
    this
  }

  override def estimate:distributions.EstimatedBinomial = distributions.EstimatedBinomial(
    estimatedRange,
    distributions.Binomial(
      trialCount,
      estimatedMean / trialCount
    ),
    s0
  )


  override inline def estimatedMean: Double = (BigDecimal(s1.total) / BigDecimal(s0)).toDouble

  override inline def estimatedRange: Interval[Long] = `[]`(min, MAX)

  override inline def totalSampleMass: Long = s0
}

class Binomial extends OnlineProbabilityDistributionEstimator[Long, distributions.Binomial] with OnlineBivariateEstimator[Long] {

  private var s0: Long = 0L
  private val s1: DiscreteAccumulator = new DiscreteAccumulator
  private val s2: DiscreteAccumulator = new DiscreteAccumulator

  private var minSuccessCount: Long = Long.MaxValue
  private var successCountMAX: Long = Long.MinValue

  override def observe(experimentCount:Long, successCount:Long, trialCount:Long): this.type = {

    s0 += experimentCount
    s1.observeProduct(successCount, experimentCount)
    s2.observeProduct(trialCount, experimentCount)

    minSuccessCount = Math.min(minSuccessCount, successCount)
    successCountMAX = Math.max(successCountMAX, successCount)

    this
  }

  def estimate:distributions.EstimatedBinomial = distributions.EstimatedBinomial(
    `[]`(minSuccessCount, successCountMAX),
    distributions.Binomial(
      (BigDecimal(s2.total) / BigDecimal(s0)).toLong, // estimated trial count per experiment
      ( BigDecimal(s1.total) / BigDecimal(s2.total) ).toDouble  // estimated Probability of success per trial.
    ),
    s0
  )

  override def observe(successCount:Long, trialCount:Long): Binomial.this.type = observe(1L, successCount, trialCount)

  override def totalSampleMass: Long = s0
}
