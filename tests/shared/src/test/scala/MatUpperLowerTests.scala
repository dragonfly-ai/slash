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

import slash.matrix.*
import slash.vector.*

class MatUpperLowerTest extends munit.FunSuite {

  test("verify correct upper triangular wide matrix") {
    val mat = Mat[3,4](
      1,2,3,4,
      3,2,4,6,
      1,3,6,9,
    )
    val expected = Mat[3,4](
      1,2,3,4,
      0,2,4,6,
      0,0,6,9,
    )
    // printf("mat:\n%s\n", mat)
    val tri = mat.upperTriangular
    // printf("tri.upperTriangular:\n%s\n", tri)
    assert(tri.strictEquals(expected))
  }
  test("verify correct lower triangular wide matrix") {
    val mat = Mat[3,4](
      1,2,3,4,
      6,7,8,9,
      1,3,6,9,
    )
    val expected = Mat[3,4](
      1,0,0,0,
      6,7,0,0,
      1,3,6,0,
    )
    // printf("mat:\n%s\n", mat)
    val tri = mat.lowerTriangular
    // printf("mat.lowerTriangular:\n%s\n", tri)
    assert(tri.strictEquals(expected))
  }
  test("verify correct upper triangular tall matrix") {
    val mat = Mat[4,3](
      1,2,3,
      4,5,6,
      7,3,2,
      4,6,8,
    )
    val expected = Mat[4,3](
      1,2,3,
      0,5,6,
      0,0,2,
      0,0,0,
    )
    // printf("mat tall:\n%s\n", mat)
    val tri = mat.upperTriangular
    // printf("mat.upperTriangular:\n%s\n", tri)
    assert(tri.strictEquals(expected))
  }
  test("verify correct lower triangular tall matrix") {
    val mat = Mat[4,3](
      1,2,3,
      4,5,6,
      7,3,2,
      4,6,8,
    )
    val expected = Mat[4,3](
      1,0,0,
      4,5,0,
      7,3,2,
      4,6,8,
    )
    // printf("mat:\n%s\n", mat)
    val tri = mat.lowerTriangular
    // printf("mat.lowerTriangular:\n%s\n", tri)
    assert(tri.strictEquals(expected))
  }
  test("verify correct upper triangular square matrix") {
    val mat = Mat[3,3](
      1,2,3,
      4,5,6,
      7,3,2,
    )
    val expected = Mat[3,3](
      1,2,3,
      0,5,6,
      0,0,2,
    )
    // printf("mat square:\n%s\n", mat)
    val tri = mat.upperTriangular
    // printf("mat.upperTriangular:\n%s\n", tri)
    assert(tri.strictEquals(expected))
  }
  test("verify correct lower triangular square matrix") {
    val mat = Mat[3,3](
      1,2,3,
      4,5,6,
      7,3,2,
    )
    val expected = Mat[3,3](
      1,0,0,
      4,5,0,
      7,3,2,
    )
    // printf("mat:\n%s\n", mat)
    val tri = mat.lowerTriangular
    // printf("mat.lowerTriangular:\n%s\n", tri)
    assert(tri.strictEquals(expected))
  }

  test("verify correct diagnonal vector for wide matrix") {
    val mat = Mat[3,4](
      1,2,3,4,
      3,2,4,6,
      1,3,6,9,
    )
    val expected = Vec[3](1,2,6)
    // printf("mat:\n%s\n", mat)
    val diag: Vec[3] = mat.diagonalVector
    // printf("mat.diagonalVector:\n%s\n", diag)
    assertVecEquals(diag, expected)
  }
  test("verify correct diagnonal vector for tall matrix") {
    val mat = Mat[4,3](
      1,2,4,
      3,2,6,
      1,6,9,
      2,3,4,
    )
    val expected = Vec[3](1,2,9)
    // printf("mat:\n%s\n", mat)
    val diag: Vec[3] = mat.diagonalVector
    // printf("mat.diagonalVector:\n%s\n", diag)
    assertVecEquals(diag, expected)
  }
  test("verify correct diagnonal vector for square matrix") {
    val mat = Mat[3,3](
      3,2,6,
      1,6,9,
      2,3,4,
    )
    val expected = Vec[3](3,6,4)
    // printf("mat:\n%s\n", mat)
    val diag: Vec[3] = mat.diagonalVector
    // printf("expected:\n%s\n", expected)
    // printf("mat.diagonalVector:\n%s\n", diag)
    assertVecEquals(diag, expected)
  }
}
