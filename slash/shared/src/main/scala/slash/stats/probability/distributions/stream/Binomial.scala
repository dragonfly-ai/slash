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

import scala.language.postfixOps
import scala.language.implicitConversions

case class FixedBinomial(trialCount: Long) extends OnlineProbabilityDistributionEstimator[Long, distributions.Binomial] with EstimatesBoundedMean[Long] {

  private val s0: DiscreteAccumulator = DiscreteAccumulator()
  private val s1: DiscreteAccumulator = DiscreteAccumulator()

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
    sampleRange,
    distributions.Binomial(
      trialCount,
      sampleMean / trialCount
    ),
    BigDecimal(s0.total)
  )


  override inline def sampleMean: Double = (s1 / s0).total.toDouble

  override inline def sampleRange: Interval[Long] = `[]`(min, MAX)

  override inline def sampleMass: BigDecimal = BigDecimal(s0.total)
}

class Binomial extends OnlineProbabilityDistributionEstimator[Long, distributions.Binomial] with OnlineBivariateEstimator[Long] {

  private val s0: DiscreteAccumulator = DiscreteAccumulator()
  private val s1: DiscreteAccumulator = DiscreteAccumulator()
  private val s2: DiscreteAccumulator = DiscreteAccumulator()

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
      (s2 / s0).total.toLong, // estimated trial count per experiment
      ( BigDecimal(s1.total) / BigDecimal(s2.total) ).toDouble  // estimated Probability of success per trial.
    ),
    BigDecimal(s0.total)
  )

  override def observe(successCount:Long, trialCount:Long): Binomial.this.type = observe(1L, successCount, trialCount)

  override def sampleMass: BigDecimal = BigDecimal(s0.total)
}
