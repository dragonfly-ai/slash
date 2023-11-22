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
import stats.*
import slash.interval.*
import slash.interval.{Domain, Interval}

import scala.language.postfixOps

object Uniform {

  def apply(b1:Double, b2:Double): Uniform = {
    Uniform(`[]`(Math.min(b1, b2), Math.max(b1, b2)))
  }

  lazy val domain:Domain[Double] = Domain.ℝ_Double

}

case class Uniform(interval:Interval[Double]) extends ParametricProbabilityDistribution[Double] {

  private lazy val `MAX-min`:Double = interval.MAX - interval.min
  private lazy val `1 / (MAX-min)`:Double = 1.0 / `MAX-min`

  override val μ:Double = (interval.min + interval.MAX) / 2.0

  override lazy val `σ²`:Double = ( `MAX-min` * `MAX-min` ) / 12.0
  override lazy val σ:Double = Math.sqrt(`σ²`)

  override def p(x:Double):Double = if (interval.rangeContains(x)) `1 / (MAX-min)` else 0.0

  override def random(r:scala.util.Random = slash.Random.defaultRandom): Double = {
    interval.random(r)
  }

  override def toString: String = s"Uniform( min = ${interval.min}, μ = $μ, MAX = ${interval.MAX}, σ² = ${`σ²`}, σ = $σ )"

}


case class EstimatedUniform(override val idealized: Uniform, override val sampleMass:BigDecimal) extends EstimatedProbabilityDistribution[Double, Uniform]{
  override val bounds:Interval[Double] = idealized.interval
  override def toString: String = s"UniformEstimate(min = ${bounds.min}, μ = $μ, MAX = ${bounds.MAX}, σ² = ${`σ²`}, σ = $σ, ℕ = $ℕ)"
}
