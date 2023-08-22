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

package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.stats.*
import probability.distributions



class Gaussian extends OnlineUnivariateProbabilityDistributionEstimator[Double, distributions.Gaussian]  {

  val estimator: PointStatisticsEstimator[Double] = new PointStatisticsEstimator[Double](distributions.Gaussian.domain)

  override def observe(frequency: Double, observation: Double): Gaussian = {
    estimator.observe(Array[Double](frequency, observation))
    this
  }

  override def estimate:distributions.EstimatedGaussian = {
    val sps:PointStatistics[Double] = estimator.samplePointStatistics
    distributions.EstimatedGaussian(
      sps.bounds,
      distributions.Gaussian(sps.μ, sps.`σ²`),
      sps.ℕ
    )
  }

}
