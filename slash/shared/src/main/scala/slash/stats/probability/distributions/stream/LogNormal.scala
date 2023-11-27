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
import slash.accumulation.ContinuousAccumulator

class LogNormal extends OnlineProbabilityDistributionEstimator[Double, distributions.LogNormal] with EstimatesPointStatistics[Double] {

  private val s0: ContinuousAccumulator = ContinuousAccumulator()
  private val s1: ContinuousAccumulator = ContinuousAccumulator()
  private val s2: ContinuousAccumulator = ContinuousAccumulator()

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
    sampleRange,
    distributions.LogNormal(sampleMean, sampleVariance),
    s0.total.toDouble
  )

  private inline def gaussianVariance:Double = {
//    (((s0 * s2) - (s1 * s1)) / (s0 * (s0 - 1.0))).total.toDouble
    val t0: BigDecimal = s0.total
    val t1: BigDecimal = s1.total
    ( (( t0 * s2.total ) - ( t1 * t1 )) / ( t0 * ( t0 - ContinuousAccumulator.One ) ) ).toDouble
  }

  private inline def gaussianMean:Double = (s1 / s0).total.toDouble
  override def sampleMean: Double = Math.exp(gaussianMean + (gaussianVariance / 2.0))

  override def sampleRange: Interval[Double] = `[]`(Math.exp(min), Math.exp(MAX))

  override def sampleVariance: Double = {
    val `Gσ²`:Double = gaussianVariance
    (Math.exp(`Gσ²`) - 1.0) * Math.exp(2.0 * gaussianMean + `Gσ²`)
  }

  override def sampleMass: BigDecimal = s0.total
}
