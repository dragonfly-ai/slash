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

class Poisson extends OnlineProbabilityDistributionEstimator[Long, distributions.Poisson] with EstimatesBoundedMean[Long] {

  private val s0: DiscreteAccumulator = DiscreteAccumulator()
  private val s1: DiscreteAccumulator = DiscreteAccumulator()

  private var min: Long = Long.MaxValue
  private var MAX: Long = Long.MinValue

  override def observe(observation: Long): this.type = observe(1L, observation)

  override def observe(frequency: Long, observation: Long): this.type = {
    s0 += frequency
    s1.observeProduct(observation, frequency)

    min = Math.min(min, observation)
    MAX = Math.max(MAX, observation)
    this
  }

  override def estimate:distributions.EstimatedPoisson = {
    val bμ = sampleBoundedMean
    distributions.EstimatedPoisson(
      bμ.bounds,
      distributions.Poisson(bμ.μ),
      BigDecimal(s0.total)
    )
  }

  override inline def sampleMean: Double = (s1 / s0).total.toDouble

  override inline def sampleRange: Interval[Long] = `[]`(min, MAX)

  override def sampleMass: BigDecimal = BigDecimal(s0.total)
}

case class PoissonDistributionUndefinedForNegativeNumbers(negative:Long) extends Exception(s"Poisson distribution undefined for observation: $negative")