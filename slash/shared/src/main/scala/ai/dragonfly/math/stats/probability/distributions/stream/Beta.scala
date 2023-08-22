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


class Beta extends OnlineUnivariateProbabilityDistributionEstimator[Double, distributions.Beta]  {

  val estimator: PointStatisticsEstimator[Double] = new PointStatisticsEstimator[Double](distributions.Beta.domain)

  override def observe(frequency: Double, observation: Double): Beta = {
    estimator.observe( Array[Double](frequency, observation) )
    this
  }

  override def estimate:distributions.EstimatedBeta = {
    val sps:PointStatistics[Double] = estimator.samplePointStatistics
    distributions.EstimatedBeta(
      distributions.Beta.fromMeanVarianceMinMax(sps.μ, sps.`σ²`, sps.min, sps.MAX),
      sps.ℕ
    )
  }

}

/*
NaN values for:

		"weighted": 0.25,
		"close": 0.2466,
		"high": 0.2509,
		"low": 0.1235,
		"open": 0.1328,
		"volume": 15609373.78

and

		"weighted": 0.38,
		"close": 0.3828,
		"high": 0.4011,
		"low": 0.3808,
		"open": 0.4139,
		"volume": 5823196.09
 */