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

class MatrixExtensionsTest extends munit.FunSuite {
  test("Matrix[1, N] -> Vec[N] -> Matrix[1, N]") {
    val m1x1:Matrix[1,1] = Matrix.random[1,1]()
    val m1x1Vec:Vec[1] = m1x1.asVector
    assertMatrixEquals[1, 1]( m1x1, m1x1Vec.asRowMatrix )
    assertMatrixEquals[1, 1]( m1x1, m1x1Vec.asColumnMatrix )

    type N = 42
    val m: Matrix[1, N] = Matrix.random[1, N]()
    val mVec: Vec[N] = m.asVector
    assertMatrixEquals[1, N](m, mVec.asRowMatrix)

  }
}
