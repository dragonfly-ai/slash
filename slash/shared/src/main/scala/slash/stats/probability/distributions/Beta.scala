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
import slash.interval.*
import stats.*

object Beta {

  def fromMeanVarianceMinMax(μ:Double, `σ²`:Double, min:Double = 0.0, MAX:Double = 1.0):Beta = {
    val scale:Double = MAX - min
    val μS:Double = (μ - min) / scale

    val `σ²S`:Double = `σ²` / squareInPlace(scale)
    val `1-μS`:Double = 1.0 - μS

    val α = μS * (((μS * `1-μS`)/`σ²S`) - 1.0)
    val β = `1-μS` * (((μS * `1-μS`)/`σ²S`) - 1.0)

    Beta(α, β, min, MAX)
  }

  val domain:Domain[Double] = Domain.ℝ_Double
}

case class Beta(α:Double, β:Double, min:Double = 0.0, MAX:Double = 1.0) extends ParametricProbabilityDistribution[Double] {
  def alpha:Double = α
  def beta:Double = β

  private def transform(x:Double):Double = (x * scale) + min

  // constants:
  private val scale:Double = MAX - min
  private val αβ:Double = α * β
  private val `α+β`:Double = (α + β)
  private val `(α+β)²`:Double = `α+β` * `α+β`
  private lazy val `B(α,β)`:Double = (Γ(α) * Γ(β)) / Γ(`α+β`)

  override val μ:Double = transform(α / (`α+β`))
  override val `σ²`:Double = (αβ / ( `(α+β)²` * (`α+β` + 1.0) )) * (scale * scale)
  override lazy val σ:Double = Math.sqrt(`σ²`)

  private lazy val `√( (α+β - 2) / (2αβ - α+β) )`:Double = Math.sqrt( (`α+β` - 2.0) / (2.0 * αβ - `α+β`) )
  private lazy val `min(α,β)`: Double = Math.min(α,β)
  private lazy val `max(α,β)`: Double = Math.max(α,β)
  private lazy val G:Double = `min(α,β)` + (1.0 / `√( (α+β - 2) / (2αβ - α+β) )`)
  private lazy val `log(α+β)`:Double = Math.log(`α+β`)

  // BB+BC version of Cheng's algorithm
  // ported from: https://commons.apache.org/proper/commons-math/javadocs/api-3.6.1/src-html/org/apache/commons/math3/distribution/BetaDistribution.html
  override def random(rand:scala.util.Random = slash.Random.defaultRandom): Double = {
    var w:Double = 0.0
    if (α > 1 && β > 1) { // Cheng's BB algorithm
      var continue:Boolean = true

      while (continue) {
        val u1 = rand.nextDouble()
        val u2 = rand.nextDouble()
        val v = `√( (α+β - 2) / (2αβ - α+β) )` * (Math.log(u1) - Math.log1p(-u1))
        w = `min(α,β)` * Math.exp(v)
        val z = u1 * u1 * u2
        val r = (G * v) - 1.3862944
        val s = `min(α,β)` + r - w
        continue = if (s + 2.609438 < 5 * z) {
          val t = Math.log(z)
          (s < t) && (r + `α+β` * (Math.log(`α+β`) - Math.log(`max(α,β)` + w)) < t)
        } else false
      }

      transform(if ( α == `min(α,β)`) w / (β + w) else α / (α + w))

    } else {  // Cheng's BC algorithm

      // constants
      val C1 = 1.0 / `min(α,β)`
      val D1 = 1.0 + `max(α,β)` - `min(α,β)`
      val k1 = D1 * (0.0138889 + 0.0416667 * `min(α,β)`) / (`max(α,β)` * C1 - 0.777778)
      val k2 = 0.25 + (0.5 + 0.25 / D1) * `min(α,β)`

      var continue:Boolean = true

      while (continue) {
        val u1 = rand.nextDouble()
        val u2 = rand.nextDouble()
        val y = u1 * u2
        val z = u1 * y
        var executeLast:Boolean = true
        if (u1 < 0.5) {
          executeLast = k1 > 0.25 * u2 + z - y
        } else {
          if (z <= 0.25) {
            continue = false
          } else {
            executeLast = z < k2
          }
        }
        if (executeLast) {
          val v = C1 * (Math.log(u1) - Math.log1p(-u1))
          w = `max(α,β)` * Math.exp(v)
          if (continue) {
            continue = Math.log(z) > `α+β` * (`log(α+β)` - Math.log(`min(α,β)` + w) + v) - 1.3862944
          }
        }
      }
      transform( if ( α == `max(α,β)`) w / (β + w) else α / (α + w) )
    }
    //Precision.equals(a, a0) ? w / (b + w) : b / (b + w);
    //Precision.equals(a, a0) ? w / (b + w) : b / (b + w);
  }

  override def toString: String = s"Beta(α = $α, β = $β, min = $min, MAX = $MAX μ = $μ, σ² = ${`σ²`}, σ = $σ)"

  override def p(x: Double):Double = {
    val x1:Double = (x - min) / scale
    (Math.pow(x1, α - 1.0) * Math.pow(1.0 - x1, β - 1.0)) / (scale * `B(α,β)`)
  }
}

object EstimatedBeta {
  def apply(ps: SamplePointStatistics[Double]): EstimatedBeta = EstimatedBeta(
    Beta.fromMeanVarianceMinMax(
      ps.μ,
      ps.`σ²`,
      ps.min,
      ps.MAX
    ),
    ps.ℕ
  )
}
case class EstimatedBeta(override val idealized: Beta, override val sampleMass:BigDecimal) extends EstimatedProbabilityDistribution[Double, Beta]{
  def α:Double = idealized.α
  def β:Double = idealized.β

  override val bounds: Interval[Double] = `[]`(idealized.min, idealized.MAX)

  override def toString: String = s"BetaEstimate(α = $α, β = $β, min = ${bounds.min}, MAX = ${bounds.MAX}, μ = $μ, σ² = ${`σ²`}, σ = $σ, ℕ = $ℕ)"
}