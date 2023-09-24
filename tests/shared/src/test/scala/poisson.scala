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

import ai.dragonfly.math.stats.probability.distributions.Poisson

class PoissonTests extends munit.FunSuite:
    test("Po[1]") {

      val dist1 = Poisson(1)
      assertEqualsDouble(dist1.p(0), 0.36787944117144233, 0.000000001)
      assertEqualsDouble(dist1.p(1), 0.36787944117144233 , 0.000000001)
      assertEqualsDouble(dist1.p(2), 0.18393972058572114 , 0.000000001)
      assertEqualsDouble(dist1.p(3), 0.06131324019524039 , 0.000000001)
      assertEqualsDouble(dist1.p(4), 0.015328310048810101 , 0.000000001)
      assertEqualsDouble(dist1.p(5), 0.00306566200976202 , 0.000000001)

    }

    test("dispersion"){

      val mean = 10.0

      val dist1 = Poisson(mean)

      assertEqualsDouble(dist1.μ, mean,0.00001)
      assertEqualsDouble(dist1.`σ²`, mean, 0.00001)

      val rand = ai.dragonfly.math.Random.defaultRandom
      val sample = dist1.sample[100000](100000)(rand)

      assertEqualsDouble( sample.mean, mean, 0.2)
      assertEqualsDouble( sample.variance, mean, 0.2)
      assertEqualsDouble( sample.variance / mean,1.0, 0.1 )

    }

end PoissonTests