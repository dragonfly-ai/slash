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

import ai.dragonfly.math.stats.probability.distributions
import ai.dragonfly.math.*

import scala.language.postfixOps
import scala.language.implicitConversions

class Poisson extends OnlineUnivariateProbabilityDistributionEstimator[Long, distributions.Poisson] {

  val estimator = new BoundedMeanEstimator[Long](distributions.Poisson.domain)

  override def observe(frequency: Long, observation: Long):Poisson = {
    estimator.observe(Array[Long](frequency, observation))
    this
  }

  override def estimate:distributions.EstimatedPousson = {
    val bμ = estimator.sampleBoundedMean
    distributions.EstimatedPousson(
      bμ.bounds,
      distributions.Poisson(bμ.μ),
      bμ.ℕ
    )
  }

}

case class PoissonDistributionUndefinedForNegativeNumbers(negative:Long) extends Exception(s"Poisson distribution undefined for observation: $negative")