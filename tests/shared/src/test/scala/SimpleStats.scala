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

import ai.dragonfly.math.vector.Vec
import narr.NArray

class SimpleStats extends munit.FunSuite:

   test("Some basic properties") {

      val v = Vec.fromTuple(1.0, 2.0, 3.0, 4.0, 5.0)
      val v2 = Vec.fromTuple(1.0, 2.0, 3.0, 4.0, 5.0)

      assertEquals(v.dimension, v2.dimension )

      // Reference, not value equality!
      assertNotEquals(v, v2 )
   }

    test("sum") {
      val v = Vec[5](1.0,2,3,4,5.0)
      assertEquals(v.sum , 15.0)
    }

   test("sample mean") {
    val v = Vec[5](1.0,2,3,4,5.0)
    assertEquals(v.mean , 3.0)
   }

   test("sample variance and std") {
      val v = Vec.fromTuple(2.0,4.0,4.0,4.0,5.0,5.0,7.0,9.0)
      assertEqualsDouble(v.variance, 4.571429, 0.00001)
      assertEqualsDouble(v.stdDev, 2.13809, 0.00001)
   }

   test("sample covariance") {
    // Sample version
    // https://corporatefinanceinstitute.com/resources/data-science/covariance/

    val vector1 = Vec.fromTuple(1692.0, 1978.0, 1884.0, 2151.0, 2519.0)
    val vector2 = Vec.fromTuple(68.0, 102.0, 110.0, 112.0, 154.0)

    val result = vector1.covariance(vector2)
    println(result)
    assertEqualsDouble(result, 9107.3, 0.001)

   }

   test("pearson correlation coefficient") {
    // https://www.statisticshowto.com/probability-and-statistics/correlation-coefficient-formula/
    val v1 = Vec.fromTuple(43.0, 21.0, 25.0, 42.0, 57.0, 59.0)
    val v2 = Vec.fromTuple(99.0, 65.0, 79.0, 75.0, 87.0, 81.0)
    assertEqualsDouble(v1.pearsonCorrelationCoefficient(v2), 0.529809, 0.0001)

   }

   test("element rank") {
      val v = Vec.fromTuple(1.0, 5.0, 3.0, 6.0, 1.0, 5.0)
      /*
      1.0 is the first, but has as tied rank. Take the average - 1.5
      */
      assertEquals(v.elementRanks.csv(),  Array[Double](1.5,4.5,3.0,6.0,1.5,4.5).mkString(","))
   }

   test("spearmans rank") {
    // https://statistics.laerd.com/statistical-guides/spearmans-rank-order-correlation-statistical-guide-2.php
    val v1 = Vec.fromTuple(56.0, 75.0, 45.0, 71.0, 62.0, 64.0, 58.0, 80.0, 76.0, 61.0)
    val v2 = Vec.fromTuple(66.0, 70.0, 40.0, 60.0, 65.0, 56.0, 59.0, 77.0, 67.0, 63.0)
    assertEqualsDouble(v1.spearmansRankCorrelation(v2), 0.6727, 0.001 )

    // https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient

    val v3 = Vec[10](86.0, 97.0, 99.0, 100.0, 101.0, 103.0, 106.0, 110.0, 112.0, 113.0)
    val v4 = Vec[10](2, 20.0, 28.0, 27.0, 50.0, 29.0, 7.0, 17.0, 6.0, 12.0)
    assertEqualsDouble(-0.1757575, v3.spearmansRankCorrelation(v4), 0.000001)
   }

end SimpleStats