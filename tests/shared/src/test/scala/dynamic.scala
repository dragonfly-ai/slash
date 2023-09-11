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

import ai.dragonfly.math.vector.dynamic.*

import ai.dragonfly.idx.Index
import ai.dragonfly.idx.*

import narr.NArray

class DynamicTests extends munit.FunSuite:

  test("Different vector lengths throw errors") {
    val vec2 = Vec.zeros[2]
    val vec3 = Vec.zeros[3]

    interceptMessage[ai.dragonfly.math.UnsupportedVectorDimension]("Expected Vector dimension: 3, but observed: 2"){
      vec2 +! vec3
    }
  }

  test("same length vectors can be added") {
    val vec2 = Vec.zeros[2]
    val vec3 = Vec.zeros[3]
    val idx = Index.none[3]
    idx(0) = true
    idx(1) = true
    val anotherVec2 = vec3(idx)
    val sum = vec2 +! anotherVec2

    assert(
      // notice the difference in the operator on the last line vs above
      compileErrors("""
    val vec2 = Vec.zeros[2]
    val vec3 = Vec.zeros[3]
    val idx = Index.none[3]
    idx(0) = true
    idx(1) = true
    val anotherVec2 = vec3(idx)
    val sum = vec2 + anotherVec2
    """).contains("Required: ai.dragonfly.math.vector.Vec[(2 : Int)]") )


    assertEquals(sum.dimension, 2)
    assertEquals(sum.sum, 0.0)

  }

end DynamicTests