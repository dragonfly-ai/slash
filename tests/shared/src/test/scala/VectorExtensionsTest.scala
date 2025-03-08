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
import Vec.*
import slash.matrix.*

class VectorExtensionsTest extends munit.FunSuite {

  test("Vec[N] -> Mat[1, N] -> Vec[N]") {
    val v1:Vec[7] = Vec.random[7]
    val mR:Mat[7, 1] = v1.asColumnMatrix
    val mC:Mat[1, 7] = v1.asRowMatrix
    assertVecEquals(v1, mR.asVector)
    assertVecEquals(v1, mC.asVector)
  }
}
