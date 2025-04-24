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

import narr.NArray
import slash.matrix.*

class MatStringTest extends munit.FunSuite {

  test("can reconstruct a Mat from the toString representation") {
    val mat1 = Mat.random[10,7](-1.0, 2.0)
    val mat2 = Mat[10,7](mat1.toString)
    assert(mat2.strictEquals(mat1))
  }
  test("can create matrix from String data") {
    val (rows, cols) = (19, 11)
    val test: Mat[rows.type,cols.type] = Mat.random[rows.type,cols.type](-1.0, 2.0)
    val mat1 = Mat.fromString(test.toString)
    assert(mat1.rows == rows && mat1.columns == cols)
  }
}
