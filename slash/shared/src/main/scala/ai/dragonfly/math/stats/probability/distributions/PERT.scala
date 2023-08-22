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


import ai.dragonfly.math.Random
import ai.dragonfly.math.stats.*
import ai.dragonfly.math.interval.*
import Interval.*
import ai.dragonfly.math.stats.BoundedMean

object PERT {

  def apply(min:Double, μ:Double, MAX:Double):PERT = PERT(
    BoundedMean[Double](μ, `[]`[Double](min, MAX), 1.0)
  )

  val domain:Domain[Double] = Domain.ℝ_Double

}

case class PERT(boundedMean:BoundedMean[Double]) extends ParametricProbabilityDistribution[Double] {

  val interval:Interval[Double] = boundedMean.bounds
  override val μ:Double = boundedMean.μ
  override val `σ²`:Double = ((μ - interval.min) * (interval.MAX - μ)) / 7.0
  override lazy val σ:Double = Math.sqrt(`σ²`)

  private lazy val underlying:Beta = Beta.fromPERT(this)

  override def p(x:Double):Double = underlying.p(x)

  override def random(r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom): Double = underlying.random(r)

  override def toString: String = s"PERT( min = ${interval.min}, μ = $μ, MAX = ${interval.MAX}, σ² = ${`σ²`}, σ = $σ )"

}


case class EstimatedPERT(override val idealized: PERT, override val ℕ:Double) extends EstimatedProbabilityDistribution[Double, PERT]{
  override val interval = idealized.interval
  override def toString: String = s"EstimatedPERT(min = ${interval.min},  μ = $μ, MAX = ${interval.MAX}, σ² = ${`σ²`}, σ = $σ, ℕ = $ℕ)"
}