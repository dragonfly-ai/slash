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
import interval.*

object Poisson {
  val domain:Domain[Long] = Domain.ℕ_Long
}

case class Poisson(λ:Double) extends ParametricProbabilityDistribution[Long] {

  def lambda:Double = λ
  override val μ:Double = λ
  override val `σ²`:Double = λ
  lazy val σ:Double = Math.sqrt(λ)

  def p(x:Long):Double = Math.exp( x.toDouble * Math.log(λ) - λ - Math.log(Γ(x.toDouble+1.0)) )

  // Knuth's method:
  override def random(r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom): Long = {
    val L = BigDecimal(Math.pow(Math.E, -λ))
    var k = 0L
    var p = 1.0
    while (p > L) {
      k = k + 1L
      p = p * r.nextDouble()
    }
    k - 1
  }
//Poissonλ = μ = σ² = $λ, σ = √λ = $σ, n = $s0)
  override def toString: String = s"Poisson(λ = μ = σ² = $λ, √λ = $σ)"
}

case class EstimatedPousson(override val interval:Interval[Long], override val idealized: Poisson, override val ℕ:Long) extends EstimatedProbabilityDistribution[Long, Poisson]{
  def λ:Double = idealized.λ
  def sampleLambda:Double = idealized.λ

  override def toString: String = s"PoissonEstimate(min = ${interval.min}, MAX = ${interval.MAX}, λ = μ = σ² = $λ, √λ = $σ, ℕ = $ℕ)"
}