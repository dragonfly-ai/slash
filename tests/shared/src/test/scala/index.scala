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
import ai.dragonfly.math.vector.Vec.*

import ai.dragonfly.math.vector.dynamic.*

import ai.dragonfly.idx.Index
import ai.dragonfly.idx.*

import narr.NArray

class IndexTests extends munit.FunSuite:

  lazy val v_fill = Vec.tabulate[5](i => i.toDouble )

  println(v_fill.render())

  test(" ways of making index ") {
    val v_idx: Index[5] = Index.none[5]

    val noResults = v_fill(v_idx)
    assertEquals(noResults.dimension, 0)

    v_idx.changeAt(1, true)
    v_idx.changeAt(2, true)

    assertEquals(v_idx.countTrue, 2)

    val indexed = v_fill(v_idx)
    assertEquals(indexed.dimension, 2)
    assertEquals(indexed(0), 1.0)
    assertEquals(indexed(1), 2.0)


  }

  test("<=") {
    val v_idx2: Index[5] = v_fill < 2.5
    assertEquals(v_idx2.countTrue, 3)
  }

  test("<") {
    val v_idx2: Index[5] = v_fill < 3.0
    assertEquals(v_idx2.countTrue, 3)
  }

  test(">") {
    val v_idx2: Index[5] = v_fill > 2.5
    assertEquals(v_idx2.countTrue, 2)
  }

  test(">=") {
    val v_idx2: Index[5] = v_fill >= 3.0
    assertEquals(v_idx2.countTrue, 2)
  }

  test("&&") {
    val v_idx2: Index[5] = (v_fill < 3.0) && (v_fill > 1.0)
    assertEquals(v_idx2.countTrue, 1)
  }

  test("||") {
    val v_idx2: Index[5] = (v_fill < 3.0) || (v_fill > 1.0)
    assertEquals(v_idx2.countTrue, 5)

    val v_idx3: Index[5] = (v_fill < 1.0) || (v_fill > 4.0)
    assertEquals(v_idx3.countTrue, 1)
  }


end IndexTests