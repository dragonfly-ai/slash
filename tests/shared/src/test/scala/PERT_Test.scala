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

import slash.stats.probability.distributions.{PERT, Beta}

class PERT_Test extends munit.FunSuite {

  test("pert.p(x) and p.asBeta.p(x)") {

    val p:PERT = PERT(0.0, 0.5, 1.0)
    val b:Beta = p.asBeta

    assertEqualsDouble(p.μ, b.μ, 0.000000001)
    assertEqualsDouble(p.`σ²`, b.`σ²`, 0.000000001)

    var i:Int = 0; while (i < 99) {
      val r0:Double = p.random()
      val r1:Double = p.asBeta.random()
      assertEqualsDouble(p.p(r0), b.p(r0), 0.000000001)
      assertEqualsDouble(p.p(r1), b.p(r1), 0.000000001)
      i += 1
    }

  }

}
