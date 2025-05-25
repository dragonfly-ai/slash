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

import slash.interval.*
import slash.stats.*
import probability.distributions
import slash.accumulation.ContinuousAccumulator
import slash.stats.probability.distributions.SamplePointStatistics

class Gaussian extends OnlineProbabilityDistributionEstimator[Double, distributions.Gaussian] with EstimatesPointStatistics[Double] {

  private val s0: ContinuousAccumulator = ContinuousAccumulator()
  private val s1: ContinuousAccumulator = ContinuousAccumulator()
  private val s2: ContinuousAccumulator = ContinuousAccumulator()

  private var min: Double = Double.MaxValue
  private var MAX: Double = Double.MinValue

  override def observe(observation: Double): this.type = observe(1.0, observation)

  override def observe(frequency: Double, observation: Double): this.type = {
    val weighted: Double = frequency * observation
    s0 += frequency // sample size
    s1 += weighted // sample sum
    s2.observeProduct(weighted, weighted) // sum of weighted samples squared
    min = Math.min(observation, min) // min
    MAX = Math.max(observation, MAX) // MAX
    this
  }

  override def estimate:distributions.EstimatedGaussian = {
    val sps:SamplePointStatistics[Double] = samplePointStatistics
    distributions.EstimatedGaussian(
      sps.bounds,
      distributions.Gaussian(sps.μ, sps.`σ²`),
      sps.ℕ
    )
  }

  override inline def sampleMean: Double = (s1 / s0).total.toDouble

  override inline def sampleRange: Interval[Double] = `[]`(min, MAX)

  override inline def sampleVariance: Double = {
//    (((s0 * s2) - (s1 * s1)) / (s0 * (s0 - 1.0))).total.toDouble
    val t0: BigDecimal = s0.total
    val t1: BigDecimal = s1.total
    ( ((t0 * s2.total) - (t1 * t1)) / (t0 * (t0 - ContinuousAccumulator.One))).toDouble
  }

  override inline def sampleMass: BigDecimal = s0.total
}
