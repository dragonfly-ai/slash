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
import slash.interval.Interval



class Gaussian extends OnlineProbabilityDistributionEstimator[Double, distributions.Gaussian] with EstimatesPointStatistics[Double] {

  private var s0: Double = 0.0
  private var s1: Double = 0.0
  private var s2: Double = 0.0

  private var min: Double = Double.MaxValue
  private var MAX: Double = Double.MinValue

  override def observe(observation: Double): this.type = observe(1.0, observation)

  override def observe(frequency: Double, observation: Double): this.type = {
    s0 = s0 + frequency // sample size
    s1 = s1 + observation * frequency // sample sum
    s2 = s2 + (observation * observation) * frequency // sum of weighted samples squared
    min = Math.min(observation, min) // min
    MAX = Math.max(observation, MAX) // MAX
    this
  }

  override def estimate:distributions.EstimatedGaussian = {
    val sps:PointStatistics[Double] = estimatedPointStatistics
    distributions.EstimatedGaussian(
      sps.bounds,
      distributions.Gaussian(sps.μ, sps.`σ²`),
      sps.ℕ
    )
  }

  override inline def estimatedMean: Double = s1 / s0

  override inline def estimatedRange: Interval[Double] = `[]`(min, MAX)

  override inline def estimatedVariance: Double = (s0 * s2 - s1 * s1) / (s0 * (s0 - 1.0))

  override inline def totalSampleMass: Double = s0
}
