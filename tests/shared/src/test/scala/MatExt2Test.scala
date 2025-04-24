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
import slash.vector.Vec
import scala.language.implicitConversions

class MatExt2Test extends munit.FunSuite {

  test("verify Mat scalar add is commutative") {
    val a0 = Mat[2,6](2,  3,  4,  5,  6,  7, 8,  9, 10, 11, 12, 13)
    val a1 = a0 + 2
    val a2 = 2.0 + a0
    val a3 = a0 + 2.0
    val a4 = 2 + a0
    assert(a1.strictEquals(a2) && a1.strictEquals(a3) && a1.strictEquals(a4))
  }

  test("verify Mat scalar multiply is commutative") {
    val m0 = Mat[2,6](2,  3,  4,  5,  6,  7, 8,  9, 10, 11, 12, 13)
    val m1 = m0 * 3
    val m2 = 3.0 * m0
    val m3 = m0 * 3.0
    val m4 = 3 * m0
    assert(m1.strictEquals(m2) && m1.strictEquals(m3) && m1.strictEquals(m4))
  }

  test("Can reshape matrices"){
    val m0 = Mat[2,6](
      2,  3,  4,  5,  6,  7,
      8,  9, 10, 11, 12, 13,
    )
    val expected = Mat[6,2](
       2,  3,
       4,  5,
       6,  7,
       8,  9,
      10, 11,
      12, 13,
    )
    val result = m0.reshape[6,2]
    assert(result.strictEquals(expected))
  }

  test("Mat.horzcat concatenates matrix columns"){
    val m1 = Mat[2,2](
      0,  1,
     10, 11
    )
    val m2 = Mat[2,6](
      2, 3, 4, 5, 6, 7,
     12,13,14,15,16,17
    )
    val m3 = Mat[2,1](
      8,
      18
    )
    assert(m1.rows == m2.rows && m1.rows == m3.rows)
    val all = Mat.horzcat(m1, m2, m3)

    val expected = Mat[2,9](
      0,  1,  2,  3,  4,  5,  6,  7,  8,
     10, 11, 12, 13, 14, 15, 16, 17, 18,
    )
    if (!all.strictEquals(expected)) {
      printf("all\n[%s]\n", all.toString.trim)
      printf("expected\n[%s]\n", expected.toString.trim)
    }
    assert(all.strictEquals(expected))
  }

  test("Mat.vertcat concatenates matrix rows"){
    val m1 = Mat[2,3](
       0,  1,  2,
      10, 11, 12,
    )
    val m2 = Mat[4,3](
      20, 21, 22,
      30, 31, 32,
      40, 41, 42,
      50, 51, 52,
    )
    val m3 = Mat[1,3](
      60, 61, 62,
    )
    assert(m1.columns == m2.columns && m1.columns == m3.columns)
    val all = Mat.vertcat(m1, m2, m3)

    val expected = Mat[7,3](
       0,  1,  2,
      10, 11, 12,
      20, 21, 22,
      30, 31, 32,
      40, 41, 42,
      50, 51, 52,
      60, 61, 62,
    )
    if (!all.strictEquals(expected)) {
      printf("all\n[%s]\n", all.toString.trim)
      printf("expected\n[%s]\n", expected.toString.trim)
    }
    assert(all.strictEquals(expected))
  }

  test("m.upper is as expected"){
    val m = Mat[3,3](
       1,  2,  3,
       4,  5,  6,
       7,  8,  9,
    )
    val upr = m.upper
    val expect = Mat[3,3](
       1,  2,  3,
       0,  5,  6,
       0,  0,  9,
    )
    printf("upr[%s]\n", upr.toString)
    printf("exp[%s]\n", expect.toString)
    assert(upr.strictEquals(expect))
  }

  test("m.lower is as expected"){
    val m = Mat[3,3](
       1,  2,  3,
       4,  5,  6,
       7,  8,  9,
    )
    val low = m.lower
    val expect = Mat[3,3](
       1,  0,  0,
       4,  5,  0,
       7,  8,  9,
    )
    printf("low[%s]\n", low.toString)
    printf("exp[%s]\n", expect.toString)
    assert(low.strictEquals(expect))
  }
  test("m.diagvec is as expected for a wide Mat"){
    val m = Mat[4,3](
       1,  2,  3,
       4,  5,  6,
       7,  8,  9,
      10, 11, 12,
    )
    val diag: Vec[3] = m.diagvec
    val expect = Vec[3](1,  5,  9)
    printf("diag:\n%s\n",diag.show)
    printf("expe:\n%s\n",expect.show)
    assert(diag.show == expect.show)
  }
  test("m.diagvec is as expected for a tall Mat"){
    val m = Mat[3,4](
       1,  2,  3,  4,
       5,  6,  7,  8,
       9, 10, 11, 12,
    )
    val diag: Vec[3] = m.diagvec
    val expect = Vec[3](1, 6, 11)
    printf("diag:\n%s\n",diag.show)
    printf("expe:\n%s\n",expect.show)
    assert(diag.show == expect.show)
  }

  test("linspace produces expected result"){
    val v = Vec.linspace[5](-1, +1)
    val expect = Vec[5](-1.0, -0.5, 0.0, 0.5, 1.0)
    printf("%s\n",v.show)
    printf("%s\n",expect.show)
    assert(v.asRowMatrix.strictEquals(expect.asRowMatrix))
  }

  test("rowVector extracts correct row"){
    val expect = Vec(5, 6, 7, 8)
    val m = Mat[3,4](
       1,  2,  3,  4,
       5,  6,  7,  8,
       9, 10, 11, 12,
    )
    assert(m.rowVector(1).show == expect.show)
  }
  test("columnVector extracts correct column"){
    val expect = Vec(3, 7, 11)
    val m = Mat[3,4](
       1,  2,  3,  4,
       5,  6,  7,  8,
       9, 10, 11, 12,
    )
    assert(m.columnVector(2).show == expect.show)
  }
  test("m(row,::) expression views the correct row"){
    val expect = Vec(9, 10, 11, 12)
    val m = Mat[3,4](
       1,  2,  3,  4,
       5,  6,  7,  8,
       9, 10, 11, 12,
    )
    assert(m(2, ::).show == expect.show)
  }
  test("row overwrite m(x,::) modifies the correct row"){
    val mat = Mat[3,4](
       1,  2,  3,  4,
       5,  6,  7,  8,
       9, 10, 11, 12,
    )
    mat(0,::) := Vec[4](-1,-2,-3,-4)
    val expect = Mat[3,4](
      -1, -2, -3, -4,
       5,  6,  7,  8,
       9, 10, 11, 12,
    )
    assert(mat.strictEquals(expect))
  }
  test("m(::,col) expression views the correct column"){
    val expect = Vec(2, 6, 10)
    val m = Mat[3,4](
       1,  2,  3,  4,
       5,  6,  7,  8,
       9, 10, 11, 12,
    )
    assert(m(::, 1).show == expect.show)
  }
  test("column overwrite m(::, x) modifies the correct column"){
    val mat = Mat[3,4](
       1,  2,  3,  4,
       5,  6,  7,  8,
       9, 10, 11, 12,
    )
    mat(::,1) := Vec[3](-1,-2,-3)
    val expect = Mat[3,4](
       1, -1,  3,  4,
       5, -2,  7,  8,
       9, -3, 11, 12,
    )
    assert(mat.strictEquals(expect))
  }
  test("last column is accessible via column index -1"){
    val mat = Mat[3,4](
       1,  2,  3,  4,
       5,  6,  7,  8,
       9, 10, 11, 12,
    )
    val lastcol: Vec[3] = mat(::, -1)
    val expect = Vec[3](4, 8, 12)
    assert(lastcol.show == expect.show)
  }
  test("middle row of Mat[3] is accessible via row index -2"){
    val mat = Mat[3,4](
       1,  2,  3,  4,
       5,  6,  7,  8,
       9, 10, 11, 12,
    )
    val midrow: Vec[4] = mat(-2, ::)
    val expect = Vec[4](5, 6, 7, 8)
    assert(midrow.show == expect.show)
  }
  test("flatten returns a row-major Vec"){
    val mat = Mat[3,4](
       1,  2,  3,  4,
       5,  6,  7,  8,
       9, 10, 11, 12,
    )
    val flat = mat.flatten
    val expect = Vec[12](1,  2,  3,  4, 5,  6,  7,  8, 9, 10, 11, 12)
    assert(flat.show == expect.show)
  }

}
