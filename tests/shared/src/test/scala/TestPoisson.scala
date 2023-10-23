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

import ai.dragonfly.math.stats.probability.distributions.{EstimatedPoisson, Poisson, stream}
import narr.*
import ai.dragonfly.math.vector.*

class TestPoisson extends munit.FunSuite:
    test("poisson.p(x)") {

      val dist1 = Poisson(1)
      assertEqualsDouble(dist1.p(0), 0.36787944117144233, 0.000000001)
      assertEqualsDouble(dist1.p(1), 0.36787944117144233 , 0.000000001)
      assertEqualsDouble(dist1.p(2), 0.18393972058572114 , 0.000000001)
      assertEqualsDouble(dist1.p(3), 0.06131324019524039 , 0.000000001)
      assertEqualsDouble(dist1.p(4), 0.015328310048810101 , 0.000000001)
      assertEqualsDouble(dist1.p(5), 0.00306566200976202 , 0.000000001)
    }

    test("dispersion") {

      val mean = 10.0

      val p0 = Poisson(mean)

      assertEqualsDouble(p0.μ, mean,0.00001)
      assertEqualsDouble(p0.`σ²`, mean, 0.00001)

      val poissonSample:NArray[Long] = p0.sample(100000)
      val sample:Vec[100000] = Vec.tabulate[100000]( (i:Int) => poissonSample(i).toDouble )

      assertEqualsDouble( sample.mean, mean, 0.2)
      assertEqualsDouble( sample.variance, mean, 0.2)
      assertEqualsDouble( sample.variance / mean,1.0, 0.1 )

    }

    test("estimate Poisson") {
      // generated from: Poisson(42.0)
      val observations: NArray[Long] = NArray[Long](44L, 49L, 35L, 44L, 44L, 46L, 50L, 31L, 39L, 44L, 36L, 23L, 46L, 31L, 46L, 44L, 41L, 36L, 38L, 29L, 39L, 51L, 47L, 46L, 45L, 45L, 46L, 52L, 55L, 39L, 52L, 47L, 36L, 38L, 40L, 53L, 35L, 50L, 34L, 36L, 38L, 49L, 41L, 38L, 32L, 38L, 40L, 43L, 45L, 43L, 49L, 41L, 61L, 47L, 43L, 36L, 64L, 42L, 32L, 33L, 51L, 43L, 48L, 55L, 40L, 57L, 52L, 43L, 44L, 51L, 46L, 45L, 41L, 36L, 45L, 49L, 38L, 47L, 48L, 34L, 40L, 35L, 46L, 38L, 40L, 37L, 39L, 48L, 55L, 39L, 56L, 44L, 46L, 42L, 33L, 48L, 48L, 39L, 40L)

      val sP:stream.Poisson = stream.Poisson()

      var i: Int = 0; while (i < observations.length) {
        sP.observe(observations(i))
        i += 1
      }

      val eP: EstimatedPoisson = sP.estimate

      assertEqualsDouble(eP.μ, 43.0606060606061, 0.000000001)
      assertEqualsDouble(eP.`σ²`, 43.0606060606061, 0.000000001)
      assertEqualsDouble(eP.σ, Math.sqrt(43.0606060606061), 0.000000001)
      assertEquals(sP.totalSampleMass, 99L)
    }

end TestPoisson