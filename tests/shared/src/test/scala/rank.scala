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

class VecTests extends munit.FunSuite:

   test("Some basic properties") {

      val v = Vec.fromTuple(1.0, 2.0, 3.0, 4.0, 5.0)
      val v2 = Vec.fromTuple(1.0, 2.0, 3.0, 4.0, 5.0)

      assertEquals(v.dimension, v2.dimension )

      // Reference, not value equality!
      assertNotEquals(v, v2 )
   }

   test("sample mean") {
    val v = Vec.fromTuple(2.0,4.0,4.0,4.0,5.0,5.0,7.0,9.0)
    assertEquals(v.mean, 5.0)
   }

   test("sample variance and std") {
    // https://en.wikipedia.org/wiki/Standard_deviation#Basic_examples
      val v = Vec.fromTuple(2.0,4.0,4.0,4.0,5.0,5.0,7.0,9.0)
      assertEquals(v.variance, 4.0)
      assertEquals(v.stdDev, 2.0)
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

   test("element rank") {
      val v = Vec.fromTuple(1.0, 5.0, 3.0, 6.0, 1.0, 5.0)
      /*
      1.0 is the first, but has as tied rank. Take the average - 1.5
      */
      assertEquals(v.elementRanks.csv(),  Array[Double](1.5, 4.5, 3.0, 6, 1.5, 4.5).mkString(","))

      val v2 = Vec.fromTuple(1.0, 5.0, 3.0, 6.0, 1.0, 5.0, 1.0)
      assertEquals(v2.elementRanks.csv(),  Array[Double](2, 5.5, 4.0, 7, 2, 5.5, 2).mkString(","))
   }


end VecTests