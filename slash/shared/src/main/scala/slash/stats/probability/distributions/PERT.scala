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

import slash.B
import slash.stats.*
import slash.interval.*

case class PERT(min:Double, mode:Double, MAX:Double) extends ParametricProbabilityDistribution[Double] {

  lazy val interval:Interval[Double] = `[]`(min, MAX)
  override val μ:Double = (min + 4.0 * mode + MAX) / 6.0
  override val `σ²`:Double = ((μ - interval.min) * (interval.MAX - μ)) / 7.0
  override lazy val σ:Double = Math.sqrt(`σ²`)

  private lazy val α = 1.0 + 4 * (mode - min) / (MAX - min)
  private lazy val β = 1.0 + 4 * (MAX - mode) / (MAX - min)
  private lazy val `B(α,β)*(MAX-min)^α+β-1` = B(α, β) * Math.pow(MAX - min, α + β - 1)

  override def p(x: Double): Double = {
    Math.pow(x - min, α - 1.0) * Math.pow(MAX - x, β - 1.0) / `B(α,β)*(MAX-min)^α+β-1`
  }

  lazy val asBeta:Beta = probability.distributions.Beta(α, β, min, MAX)
  override def random(r:scala.util.Random = slash.Random.defaultRandom): Double = asBeta.random(r)

  override def toString: String = s"PERT( min = ${interval.min}, μ = $μ, MAX = ${interval.MAX}, σ² = ${`σ²`}, σ = $σ )"

}
