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
import slash.matrix.*

class MatReshapeFlattenTest extends munit.FunSuite {

  test("verify Mat.reshape") {
    val mat1 = Mat.random[10,7](-1.0, 2.0)
    val mat2 = mat1.reshape[5,14]
    val mat3: Mat[5,14] = mat2 // dimensions verified by compiler
    assert(mat1.values == mat2.values && mat2.strictEquals(mat3))
  }
  test("verify flatten") {
    val mat = Mat.random[10,7](-1.0, 2.0)
    val vec: Vec[70] = mat.flatten
    assert(mat.values == vec)
  }
}
