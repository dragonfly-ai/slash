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

import slash.vector.VectorSpace

class MatConcatenateTest extends munit.FunSuite {
  test("concatenate rows of 2 matrices") {
    val mat1 = Mat[2,3](
      0,1,2,
      3,4,5
    )
    val mat2 = Mat[1,3](
      6,7,8
    )
    val expected = Mat[3,3](
      0,1,2,
      3,4,5,
      6,7,8
    )
    val allRows = mat1.concatenateRows[1](mat2)
    assert(allRows.strictEquals(expected))
  }
  test("concatenate columns of 2 matrices") {
    val mat1 = Mat[2,3](
      0,1,2,
      3,4,5
    )
    val mat2 = Mat[2,2](
      6,7,
      8,9,
    )
    val expected = Mat[2,5](
      0,1,2,6,7,
      3,4,5,8,9
    )
    val allRows = mat1.concatenateColumns(mat2)
    assert(allRows.strictEquals(expected))
  }
  test("multiply concatenated matrices") {
    val allRows: Mat[4,3] = {
      val mat1 = Mat[2,3](
        0,1,2,
        3,4,5
      )
      val mat2 = Mat[2,3](
        6,7,8,
        10,11,12
      )
      mat1.concatenateRows[2](mat2)
    }

    val allCols: Mat[3,6] = {
      val mat3 = Mat[3,3](
        0,1,2,
        3,4,5,
        6,7,8,
      )
      val mat4 = Mat[3,3](
        6,7,8,
        10,11,12,
        13,14,15
      )
      mat3.concatenateColumns(mat4)
    }

    val matProduct: Mat[4,6] = allRows * allCols

    assertEquals(
      true,
      matProduct.strictEquals(
        Mat[4,6](
          15.0, 18.0, 21.0, 36.0, 39.0, 42.0,
          42.0, 54.0, 66.0, 123.0, 135.0, 147.0,
          69.0, 90.0, 111.0, 210.0, 231.0, 252.0,
          105.0, 138.0, 171.0, 326.0, 359.0, 392.0
        )
      )
    )
  }

  import slash.Random.defaultRandom as dr
  test("matrix row concatenation with runtime dimensions"){
    val ms0 = MatrixSpace(2 + dr.nextInt(42), 2 + dr.nextInt(42))
    val ms1 = MatrixSpace(VectorSpace(2 + dr.nextInt(42)), ms0.columnVectorSpace)
    val msr = MatrixSpace(VectorSpace(ms0.rowDimension + ms1.rowDimension), ms0.columnVectorSpace)

    val m0 = ms0.ones
    val m1 = ms1.ones

    val mr:Mat[msr.M, msr.N] = msr(m0.concatenateRows(m1))

    assertEquals(true, mr.strictEquals(msr.ones))
  }

  test("matrix columns concatenation with runtime dimensions"){
    val ms0 = MatrixSpace(2 + dr.nextInt(42), 2 + dr.nextInt(42))
    val ms1 = MatrixSpace(ms0.rowVectorSpace, VectorSpace(2 + dr.nextInt(42)))
    val msr = MatrixSpace(ms0.rowVectorSpace, VectorSpace(ms0.columnDimension + ms1.columnDimension))

    val m0 = ms0.ones
    val m1 = ms1.ones

    val mr:Mat[msr.M, msr.N] = msr(m0.concatenateColumns(m1))

    assertEquals(true, mr.strictEquals(msr.ones))
  }
}
