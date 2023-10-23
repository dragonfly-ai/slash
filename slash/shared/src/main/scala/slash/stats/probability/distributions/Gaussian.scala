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
import Constant.*
import slash.interval.*
import stats.*


object Gaussian {
  val domain:Domain[Double] = Domain.ℝ_Double
}

case class Gaussian(override val μ:Double, override val `σ²`:Double) extends ParametricProbabilityDistribution[Double]  {

  lazy val σ: Double = Math.sqrt(`σ²`)

  // precomputed constants
  private lazy val `-1/(2σ²)`: Double = -1.0 / (2.0 * `σ²`)
  private lazy val `1/σ√(2π)`: Double = 1.0 / (σ * `√(2π)`)

  def z(x:Double):Double = (x - μ) / σ

  override inline def p(x: Double): Double = p2(squareInPlace(x - μ))
  def p2(magSquared: Double): Double = `1/σ√(2π)` * Math.exp(`-1/(2σ²)` * magSquared)

  override def random(r:scala.util.Random = slash.Random.defaultRandom): Double = μ + (r.nextGaussian() * σ)

  override def toString: String = s"Gaussian(μ = $μ, σ² = ${`σ²`}, σ = $σ)"
}

case class EstimatedGaussian(override val interval: Interval[Double], override val idealized: Gaussian, override val ℕ:Double) extends EstimatedProbabilityDistribution[Double, Gaussian]{
  override def toString: String = s"GaussianEstimate(min = ${interval.min}, MAX = ${interval.MAX}, μ = $μ, σ² = ${`σ²`}, σ = $σ, ℕ = $ℕ)"
}