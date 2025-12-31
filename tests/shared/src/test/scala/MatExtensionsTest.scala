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

import slash.vector.Vec
import slash.matrix.*

class MatExtensionsTest extends munit.FunSuite {

  test("Mat[1, N] -> Vec[N] -> Mat[1, N]") {
    val m1x1:Mat[1,1] = Mat.random[1,1]
    val m1x1Vec:Vec[1] = m1x1.asVector
    assertMatrixEquals[1, 1]( m1x1, m1x1Vec.asRowMatrix )
    assertMatrixEquals[1, 1]( m1x1, m1x1Vec.asColumnMatrix )

    type N = 42
    val m: Mat[1, N] = Mat.random[1, N]
    val mVec: Vec[N] = m.asVector
    assertMatrixEquals[1, N](m, mVec.asRowMatrix)

  }

  test("Same values but unequal Dimensions should not equate") {
    val mat3x2 = Mat[3,2](1, 2, 3, 4, 5, 6)
    val mat2x3 = Mat[2,3](1, 2, 3, 4, 5, 6)
    assert(!mat3x2.strictEquals(mat2x3))
  }

  test("Mat with NaN fields unequal even if NaNs are aligned") {
    val m01: Mat[1, 2] = Mat(3, Double.NaN)
    val m02: Mat[1, 2] = Mat(3, Double.NaN)
    assert(!m01.strictEquals(m02))
  }

  test("scalar multiplication") {
    val m1 = Mat[1,3](1.5, 2.5, 3.5)
    val expected = Mat[1,3](3.0, 5.0, 7.0)
    val result = m1 * 2.0
    assert(result.strictEquals(expected))
  }

  test("scalar left multiplication") {
    val m1 = Mat[1,3](1.5, 2.5, 3.5)
    val expected = Mat[1,3](3.0, 5.0, 7.0)
    val result = 2.0 * m1
    assert(result.strictEquals(expected))
  }

  test("single row Mat from numeric args representing a single row"){
    val m2 = Mat[1,4](1,2,3,4)
    assert(m2.rows == 1 && m2.columns == 4, s"m.rows[${m2.rows}] != 1 || m.columns[${m2.columns}] != 4")
  }

  test("kronecker product") {
    val m1 = Mat[1,2](1, 2)
    val m2 = Mat[2,2](1, 2, 3, 4)
    val expected = Mat[2,4](1, 2, 2, 4, 3, 4, 6, 8)
    val result = (m1.kronecker(m2))
    assert(result.strictEquals(expected))
  }

}
