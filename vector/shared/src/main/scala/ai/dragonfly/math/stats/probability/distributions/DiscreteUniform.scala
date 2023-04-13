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

package ai.dragonfly.math.stats.probability.distributions

import ai.dragonfly.math.*
import stats.*
import ai.dragonfly.math.interval.*
import Interval.*

import scala.language.postfixOps

object DiscreteUniform {
  def apply(b1:Long, b2:Long): DiscreteUniform = {
    DiscreteUniform(`[]`[Long](Math.min(b1, b2), Math.max(b1, b2)))
  }
  val domain:Domain[Long] = Domain.ℤ_Long
}

case class DiscreteUniform(interval:Interval[Long]) extends ParametricProbabilityDistribution[Long] {

  private lazy val `MAX-min`:Double = interval.MAX - interval.min.toDouble
  private lazy val `1 / (MAX-min)`:Double = 1.0 / `MAX-min`

  override val μ = (interval.min + interval.MAX).toDouble / 2.0

  override lazy val `σ²`:Double = ( (`MAX-min` * `MAX-min`) - 1.0 ) / 12.0
  override lazy val σ:Double = Math.sqrt(`σ²`)


  override def p(x:Long):Double = if (interval.contains(x)) `1 / (MAX-min)` else 0.0

  override def random(r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom): Long = interval.min + r.nextLong(interval.MAX - interval.min)

  override def toString: String = s"DiscreteUniform( min = ${interval.min}, μ = $μ, MAX = ${interval.MAX}, σ² = ${`σ²`}, σ = $σ )"

}


case class EstimatedDiscreteUniform(override val interval: Interval[Long], override val idealized: DiscreteUniform, override val ℕ:Long) extends EstimatedProbabilityDistribution[Long, DiscreteUniform]{
  override def toString: String = s"DiscreteUniformEstimate(min = ${interval.min}, μ = $μ, MAX = ${interval.MAX}, σ² = ${`σ²`}, σ = $σ, ℕ = $ℕ)"
}