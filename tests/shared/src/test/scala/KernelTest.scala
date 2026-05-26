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

import slash.stats.kernel.GaussianKernel
import slash.stats.probability.distributions.Gaussian
import slash.vector.*
import slash.vector.runtime.RTVec

class KernelTest extends munit.FunSuite {

  test("GaussianKernel") {

    val gk:GaussianKernel = GaussianKernel(9.0, Gaussian(0.0, 9.0))

    var i:Int = 0
    while (i < 1000) {
      assertEquals(gk.weight(Vec.random[3](9.0, 100.0)), 0.0)
      assertNotEquals(gk.weight(Vec.random[3](0.0, 3.0)), 0.0)
      assertEquals(gk.rtWeight(RTVec.random(3, 9.0, 100.0)), 0.0)
      assertNotEquals(gk.rtWeight(RTVec.random(3, 0.0, 3.0)), 0.0)
      i = i + 1
    }
  }

}
