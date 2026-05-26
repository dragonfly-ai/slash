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

package slash.stats.probability.distributions

import slash.*
import slash.interval.Interval

trait SampledMass {
  def sampleMass:BigDecimal
  inline def ℕ:BigDecimal = sampleMass
}

trait SampledMean {
  def sampleMean:Double
  inline def μ:Double = sampleMean
}

trait SampledVariance {
  def sampleVariance:Double
  lazy val sampleStandardDeviation:Double = `√`(sampleVariance)
  inline def σ: Double = sampleStandardDeviation
  inline def `σ²`:Double = sampleVariance
}

trait SampledBounds[DOMAIN] {
  def bounds: Interval[DOMAIN]
}

trait SampledBoundedMean[DOMAIN] extends SampledBounds[DOMAIN] with SampledMean with SampledMass
trait SampledPointStatistics[DOMAIN] extends SampledVariance with SampledBoundedMean[DOMAIN]

case class SampleMean(override val sampleMean: Double) extends SampledMean
case class SampleVariance(override val sampleVariance: Double) extends SampledVariance
case class SampleBounds[DOMAIN](override val bounds:Interval[DOMAIN]) extends SampledBounds[DOMAIN]

case class SampleMeanAndVariance (
  override val sampleMean:Double,
  override val sampleVariance:Double,
  override val sampleMass:BigDecimal
) extends SampledMass with SampledMean with SampledVariance

case class SampleBoundedMean[DOMAIN](
  override val sampleMean: Double,
  override val bounds: Interval[DOMAIN],
  override val sampleMass:BigDecimal
) extends SampledBoundedMean[DOMAIN] {
  require(
    bounds.rangeContains(μ),
    s"Cannot create SampleBoundedMean(μ = $sampleMean, bounds = $bounds).  $sampleMean lies outside of $bounds."
  )
  inline def min:DOMAIN = bounds.min
  inline def MAX:DOMAIN = bounds.MAX
}

type SampleBoundedMeanAndVariance = SamplePointStatistics.type

case class SamplePointStatistics[DOMAIN](
  override val sampleMean: Double,
  override val sampleVariance: Double,
  override val bounds: Interval[DOMAIN],
  override val sampleMass: BigDecimal
) extends SampledPointStatistics[DOMAIN] {
  require(
    bounds.rangeContains(μ),
    s"Cannot create SamplePointStatistics(μ = $sampleMean, bounds = $bounds).  $sampleMean lies outside of $bounds."
  )
  def min:DOMAIN = bounds.min
  def MAX:DOMAIN = bounds.MAX
}
