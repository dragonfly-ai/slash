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

import ai.dragonfly.math.interval.Interval

trait EstimatedProbabilityDistribution[DOMAIN: Numeric, PPD <: ParametricProbabilityDistribution[DOMAIN]] {
  val idealized: PPD

  def ℕ:DOMAIN
  def sampleSize:DOMAIN = ℕ

  def interval:Interval[DOMAIN]

  def μ: Double = idealized.μ
  def sampleMean: Double = μ

  def `σ²`: Double = idealized.`σ²`
  def sampleVariance: Double = `σ²`

  def σ: Double = idealized.σ
  def sampleStandardDeviation: Double = σ

  def p(x: DOMAIN): Double = idealized.p(x)

  def random(): DOMAIN = idealized.random()
}