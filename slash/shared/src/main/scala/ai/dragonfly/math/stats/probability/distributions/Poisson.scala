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
import ai.dragonfly.math.vector.Vec

object Poisson {
  val domain:Domain[Long] = Domain.ℕ_Long
}

/**
 * 🐟
 * @param λ
 */
case class Poisson(λ:Double) extends ParametricProbabilityDistribution[Long] {
  def lambda:Double = λ
  override val μ:Double = λ
  override val `σ²`:Double = λ
  lazy val σ:Double = Math.sqrt(λ)

  lazy private val `ln(λ)`:Double = ln(λ)
  lazy private val L:Double = Math.pow(Math.E, -λ)

  // https://en.wikipedia.org/wiki/Poisson_distribution#Computational_methods
  def p(k:Long):Double = {
    val x: Double = k.toDouble
    Math.exp(x * `ln(λ)` - λ - lnGamma(x + 1))
  }

  // Knuth's method:
  override def random(r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom): Long = {
    var k = 0L
    var p = 1.0
    while (p > L) {
      k = k + 1L
      p = p * r.nextDouble()
    }
    k - 1
  }

  inline def sample[N<:Int](n: Int)(inline r:scala.util.Random): Vec[N] = {
    val v = Vec.zeros[n.type]
    for (i <- 0 until n) {
      v(i) = random(r).toDouble
    }
    v.asInstanceOf[Vec[N]]
  }

//Poissonλ = μ = σ² = $λ, σ = √λ = $σ, n = $s0)
  override def toString: String = s"Poisson(λ = μ = σ² = $λ, √λ = $σ)"
}



case class EstimatedPoisson(override val interval:Interval[Long], override val idealized: Poisson, override val ℕ:Long) extends EstimatedProbabilityDistribution[Long, Poisson]{
  def λ:Double = idealized.λ
  def sampleLambda:Double = idealized.λ
  override def toString: String = s"PoissonEstimate(min = ${interval.min}, MAX = ${interval.MAX}, λ = μ = σ² = $λ, √λ = $σ, ℕ = $ℕ)"
}