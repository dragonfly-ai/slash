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

import slash.vector.*
import slash.stats.probability.distributions.{EstimatedGaussian, Gaussian, stream}

class GaussianTest extends munit.FunSuite {

  test("gaussian.p(x)") {

    // test values computed by: https://distribution-calc.github.io/
    val g = Gaussian(0.0, 1.0)
    assertEqualsDouble(g.p(-3.5), 0.000872682695, 0.000000001)
    assertEqualsDouble(g.p(-3.0), 0.004431848412, 0.000000001)
    assertEqualsDouble(g.p(-2.5), 0.01752830049, 0.000000001)
    assertEqualsDouble(g.p(-2.0), 0.05399096651, 0.000000001)
    assertEqualsDouble(g.p(-1.5), 0.1295175957, 0.000000001)
    assertEqualsDouble(g.p(-1.0), 0.2419707245, 0.000000001)
    assertEqualsDouble(g.p(-0.5), 0.3520653268, 0.000000001)
    assertEqualsDouble(g.p( 0.0), 0.3989422804, 0.000000001)
    assertEqualsDouble(g.p( 0.5), 0.3520653268, 0.000000001)
    assertEqualsDouble(g.p( 1.0), 0.2419707245, 0.000000001)
    assertEqualsDouble(g.p( 1.5), 0.1295175957, 0.000000001)
    assertEqualsDouble(g.p( 2.0), 0.05399096651, 0.000000001)
    assertEqualsDouble(g.p( 2.5), 0.01752830049, 0.000000001)
    assertEqualsDouble(g.p( 3.0), 0.004431848412, 0.000000001)
    assertEqualsDouble(g.p( 3.5), 0.000872682695, 0.000000001)

  }

  test("estimated Gaussian ") {

    // generated by: Gaussian(0.0, Math.PI)
    val observations:Vec[99] = Vec[99](0.743093524692193, 0.314709427492416, 0.838939764387742, 1.51496037766117, -1.43306720880795, 1.03047293923498, 0.38919393507152, -0.521673002754245, -3.63008278676088, 2.19409023467141, 1.58261025701969, -0.731816830985393, 3.18424580785301, -0.283329946019708, 0.199737770184699, -0.675105844886054, 1.14557679179107, -0.222600596176826, -2.1133204911517, 1.53000439508737, -1.12034659520025, 0.936264313220451, 1.2417264209158, 1.59922598724177, -0.133472922911635, 0.736854376050841, 1.34062124675367, 0.840169703602761, -0.193214506855496, 0.70133323147214, -1.98065072027202, 2.58316174510591, 0.923271991151195, 0.563432817463527, -1.48680510678106, 0.614217169706074, -3.66131274059812, 1.4760211551943, 1.14774051310953, -2.96867337399257, 1.89955787077905, 0.962602499371484, -0.971037522715233, -0.615568729143394, 0.140472239620646, -0.0155494702758777, 0.0590156018513416, -0.361715586316222, -0.833727136607875, 2.18112631466953, 0.435205273658177, 0.886641958104597, 1.00962389379904, 1.12798856753813, -0.514189101835297, -0.00184797876592702, -0.289891477227173, -0.898203173426415, 0.0841732887794391, 1.38996799546909, 0.418546082984325, 1.09997205976998, -2.42126638822048, 1.81144756650297, 1.27864272364095, 0.390878689419778, -1.82973329276294, 3.36646538953441, -1.42421429784644, -0.924379960482618, -2.49760087701001, -0.906811458760369, -1.85082414777356, 2.6791595156505, 2.87252574923738, -0.424753419321853, -1.01140038703932, 1.27522631554488, -3.52675547601004, -1.90907908815129, -0.850436971589662, 0.794726521056397, 0.411996281668379, -3.97328783261465, -0.79582903546755, 1.35847904098254, 1.72675057020146, 0.367235051628822, 1.86468842319714, 0.899430090663365, -1.00498384186375, 2.48777885336183, 3.83054267321334, 1.44253537299828, 0.682000577511285, 1.05587390988779, -2.04611519374751, -4.33062642547896, 2.6730613921993)

    val sG: stream.Gaussian = stream.Gaussian()
    var i: Int = 0; while (i < observations.dimension) {
      sG.observe(observations(i))
      i += 1
    }

    val eG:EstimatedGaussian = sG.estimate

    assertEqualsDouble(eG.μ, 0.17121932632346, 0.0001)
    assertEqualsDouble(eG.σ, 1.65812782420032, 0.0001)

  }
}