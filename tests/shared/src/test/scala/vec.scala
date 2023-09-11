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

class VecTests extends munit.FunSuite:
  test("Compile safety") {
    val vec2 = Vec.zeros[2]
    val vec3 = Vec.zeros[3]

    compileErrors("vec2 + vec3")
    assertEquals((vec2 + vec2).sum, 0.0)
  }

end VecTests