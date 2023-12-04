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
import narr.*

class InstantiationTest extends munit.FunSuite {

  test(" ways of making vecs ") {

    //type dim = 5

    val v = Vec.fromTuple(1.0, 2.0, 3.0, 4.0, 5.0)
    assertEquals(v.dimension, 5)
    val v2 = Vec[5](1.0, 2.0, 3.0, 4.0, 5.0)
    val v_fill = Vec.fill[5](1.0)
    val v_tabulate = Vec.tabulate[5](i => i.toDouble)

    val v_zeros = Vec.zeros[5]
    val v_ones = Vec.ones[5]
    assertEquals(v_ones.sum, 5.0)

    val v_rand = Vec.random[5]
    val v_rand_max_min = Vec.random[5](0.5, 2.0)

    assertEquals(v2.dimension, v2.dimension)
    assertEquals(v2.dimension, v_fill.dimension)
    assertEquals(v2.dimension, v_tabulate.dimension)
    assertEquals(v2.dimension, v_zeros.dimension)
    assertEquals(v2.dimension, v_rand.dimension)
    assertEquals(v2.dimension, v_rand_max_min.dimension)

  }
}
