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
import Constant.π

// https://en.wikipedia.org/wiki/Log-normal_distribution#Generation_and_parameters

object LogNormal {

  def fromGaussianParameters(Gμ:Double, `Gσ²`: Double): LogNormal = LogNormal(
    Math.exp(Gμ + (`Gσ²` / 2.0)),
    (Math.exp(`Gσ²`) - 1.0) * Math.exp(2.0 * Gμ + `Gσ²`)
  )

  inline def fromGaussian(g: Gaussian): LogNormal = fromGaussianParameters(g.μ, g.`σ²`)

  private val `√(2π)`:Double = Math.sqrt(2.0 * π)
  def p(x:Double, μG:Double, σG: Double):Double = if (x <= 0.0) 0.0 else {
    val `ln(x)-μ`:Double = Math.log(x) - μG
    (1.0 / (x * (σG * `√(2π)`))) * Math.exp(-0.5 * (squareInPlace(`ln(x)-μ`) / squareInPlace(σG)))
  }

  val domain:Domain[Double] = Domain.`ℝ+_Double`
}

case class LogNormal(override val μ:Double, override val `σ²`: Double) extends ParametricProbabilityDistribution[Double] {

  val G:Gaussian = {
    val `μ²`:Double = squareInPlace(μ)
    Gaussian(
      ln( `μ²` / Math.sqrt(`μ²` + `σ²`) ), // transform mean
      ln( 1.0 + (`σ²` / `μ²`) ) // transform variance
    )
  }

  lazy val σ: Double = Math.sqrt(`σ²`)

  def p(x:Double):Double = LogNormal.p(x, μ, σ)

  override def random(r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom): Double = {
    Math.exp(G.random(r))
  }

  override def toString: String = s"LogNormal( μ = $μ, σ² = ${`σ²`}, σ = $σ )"
}


case class EstimatedLogNormal(override val interval:Interval[Double], override val idealized: LogNormal, override val ℕ:Double) extends EstimatedProbabilityDistribution[Double, LogNormal]{
  override def toString: String = s"LogNormalEstimate(min = ${interval.min}, MAX = ${interval.MAX}, μ = $μ, σ² = ${`σ²`}, σ = $σ, ℕ = $ℕ)"
}