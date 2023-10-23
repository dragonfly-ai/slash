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
import slash.{ln, squareInPlace}
import slash.stats.probability.distributions
import slash.interval.Interval

class LogNormal extends OnlineProbabilityDistributionEstimator[Double, distributions.LogNormal] with EstimatesPointStatistics[Double] {

  private var s0: Double = 0.0
  private var s1: Double = 0.0
  private var s2: Double = 0.0

  private var min: Double = Double.MaxValue
  private var MAX: Double = Double.MinValue

  override inline def observe(observation: Double): this.type = observe(1.0, observation)

  override def observe(frequency: Double, observation: Double): this.type = {
    val lnob:Double = ln(observation)
    s0 += frequency // sample size
    s1 += lnob * frequency // sample sum
    s2 += squareInPlace(lnob) * frequency // sum of weighted samples squared

    min = Math.min(lnob, min) // min
    MAX = Math.max(lnob, MAX) // MAX
    this
  }

  override def estimate:distributions.EstimatedLogNormal = distributions.EstimatedLogNormal(
    estimatedRange,
    distributions.LogNormal(estimatedMean, estimatedVariance),
    s0
  )

  private inline def gaussianVariance:Double = (s0 * s2 - s1 * s1) / (s0 * (s0 - 1.0))
  private inline def gaussianMean:Double = s1 / s0
  override def estimatedMean: Double = Math.exp(gaussianMean + (gaussianVariance / 2.0))

  override def estimatedRange: Interval[Double] = `[]`(Math.exp(min), Math.exp(MAX))

  override def estimatedVariance: Double = {
    val `Gσ²`:Double = gaussianVariance
    (Math.exp(`Gσ²`) - 1.0) * Math.exp(2.0 * gaussianMean + `Gσ²`)
  }

  override def totalSampleMass: Double = s0
}
