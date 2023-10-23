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

import ai.dragonfly.math.interval.*
import ai.dragonfly.math.stats.PointStatistics
import ai.dragonfly.math.stats.probability.distributions.{Beta, EstimatedBeta, stream}
import ai.dragonfly.math.vector.*

class TestBeta extends munit.FunSuite {

  test("beta.p(x)") {

    // test values computed by: https://www.mathworks.com/help/stats/beta-distribution.html
    /*
     * x = 0:0.1:1;
     * y1 = betapdf(x, 2.0, 5.0);
     * fprintf('%9.9G,\n', x);
     * fprintf('%9.9G,\n', y);
     */

    val b = Beta(2.0, 5.0)
    assertEqualsDouble(b.p(0.0),0, 0.000000001)
    assertEqualsDouble(b.p(0.1),1.9683, 0.000000001)
    assertEqualsDouble(b.p(0.2),2.4576, 0.000000001)
    assertEqualsDouble(b.p(0.3),2.1609, 0.000000001)
    assertEqualsDouble(b.p(0.4),1.5552, 0.000000001)
    assertEqualsDouble(b.p(0.5),0.9375, 0.000000001)
    assertEqualsDouble(b.p(0.6),0.4608, 0.000000001)
    assertEqualsDouble(b.p(0.7),0.1701, 0.000000001)
    assertEqualsDouble(b.p(0.8),0.0384, 0.000000001)
    assertEqualsDouble(b.p(0.9),0.0027, 0.000000001)
    assertEqualsDouble(b.p(1.0),0, 0.000000001)

  }

  test("estimated Beta ") {

    // generated by: https://www.mathworks.com/help/stats/beta-distribution.html
    /*
     *  rng("default") % For reproducibility
     *  r = betarnd(2.0,5.0,100,1);
     *  fprintf('%9.9G, ', r)
     */
    val observations:Vec[99] = Vec[99](0.646625975, 0.411379232, 0.0493716033, 0.367594638, 0.786842409, 0.279039132, 0.254417025, 0.328157099, 0.503521176, 0.125944772, 0.474810568, 0.373357473, 0.34725043, 0.206354585, 0.126629921, 0.539166348, 0.352441019, 0.110675266, 0.229830211, 0.508159315, 0.152335608, 0.354779986, 0.435425675, 0.238404754, 0.126191443, 0.454697379, 0.533796966, 0.465648908, 0.472420639, 0.584760257, 0.276525737, 0.139957202, 0.490088015, 0.350574054, 0.568162116, 0.0848505584, 0.255763512, 0.206960741, 0.228983682, 0.583932399, 0.261155852, 0.229125354, 0.0729172721, 0.278884397, 0.876128323, 0.586839621, 0.0414685217, 0.0438677138, 0.101211762, 0.156395361, 0.130697626, 0.290131861, 0.246947481, 0.360878497, 0.206317257, 0.22772133, 0.162471484, 0.374508357, 0.106826305, 0.313506769, 0.485988916, 0.191463311, 0.368193682, 0.27009775, 0.243751196, 0.367798268, 0.211667127, 0.244099823, 0.15806683, 0.145887011, 0.321902092, 0.341127356, 0.0773862628, 0.234675823, 0.454173415, 0.214193653, 0.0841251611, 0.5334134, 0.334003894, 0.160589337, 0.574338265, 0.0328237845, 0.282207108, 0.363039302, 0.527123651, 0.200985116, 0.366673588, 0.314668589, 0.326683671, 0.56250878, 0.307379674, 0.283553853, 0.363876926, 0.61358079, 0.346805543, 0.331152646, 0.235543721, 0.357477814, 0.242945882)

    val sB: stream.Beta = stream.Beta()
    var i: Int = 0;
    while (i < observations.dimension) {
      sB.observe(observations(i))
      i += 1
    }

    val ps:PointStatistics[Double] = sB.estimatedPointStatistics
    val eB:EstimatedBeta = EstimatedBeta( PointStatistics[Double]( ps.μ, ps.`σ²`, `[]`(0.0, 1.0), ps.ℕ ) )

    assertEqualsDouble(eB.α, 2.08436177, 0.02)
    assertEqualsDouble(eB.β, 4.49977445, 0.02)

  }
}
