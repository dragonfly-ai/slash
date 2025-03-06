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
import slash.matrix.Matrix.*

class MatrixExtensionsTest extends munit.FunSuite {
  test("Matrix[1, N] -> Vec[N] -> Matrix[1, N]") {
    val m1x1:Matrix[1,1] = Matrix.random[1,1]
    val m1x1Vec:Vec[1] = m1x1.asVector
    assertMatrixEquals[1, 1]( m1x1, m1x1Vec.asRowMatrix )
    assertMatrixEquals[1, 1]( m1x1, m1x1Vec.asColumnMatrix )

    type N = 42
    val m: Matrix[1, N] = Matrix.random[1, N]
    val mVec: Vec[N] = m.asVector
    assertMatrixEquals[1, N](m, mVec.asRowMatrix)

  }

  test("Matrix can be initialized from Tuple literal") {
    val tup3x2 = (((1, 2), (3, 4), (5, 6)))
    val m01: Matrix[3, 2] = Matrix(tup3x2)
    val m02 = Matrix(((1, 2), (3, 4), (5, 6)))
    assert(m01.strictEquals(m02))
  }
  test("Matrix equality for Tuples with mixed number types") {
    // values selected to avoid roundoff errors
    val m01: Matrix[2, 2] = Matrix(((1, 2L), (2.0, 4f))) // Int, Long, Double, Float
    val m02: Matrix[2, 2] = Matrix(((1L, 2.0), (2f, 4))) // Long, Double, Float, Int
    assert(m01.strictEquals(m02))
  }
  test("Same values but unequal Dimensions should not equate") {
    val mat3x2 = Matrix[3,2](1, 2, 3, 4, 5, 6)
    val mat2x3 = Matrix[2,3](1, 2, 3, 4, 5, 6)
    assert(!mat3x2.strictEquals(mat2x3))
  }
  test("Single or repeating Tuple parameters should be equivalent") {
    val tup1 = (((1, 2), (3, 4), (5, 6))) // single Tuple[Tuple] arg
    val tup2 =  ((1, 2), (3, 4), (5, 6))  // repeating Tuple args
    val m01: Matrix[3, 2] = Matrix(tup1)
    val m02: Matrix[3, 2] = Matrix(tup2)
    assert(m01.strictEquals(m02))
  }
  test("Matrix with NaN fields unequal even if NaNs are aligned") {
    val m01: Matrix[1, 2] = Matrix(3, Double.NaN)
    val m02: Matrix[1, 2] = Matrix(3, Double.NaN)
    assert(!m01.strictEquals(m02))
  }
  test("Tuples with non-number fields throw IllegalArgumentException") {
    val tup1 = (((1, 2), (3, 4), ("", false))) // last row is (NaN, NaN)
    val compilerError = try {
      val mat = Matrix(tup1)
      printf("%s\n", mat)
      false // fail if exception not thrown 
    } catch {
      case _ =>
        true // as expected
    }
    assert(compilerError)
  }
  test("Tuples with jagged rows should throw IllegalArgumentException") {
    val compilerError = try {
      val mat = Matrix(((1, 2), (3, 4, 5))) // compiler error
      printf("%s\n", mat)
      false // fail if exception not thrown 
    } catch {
      case _ =>
        true // as expected
    } 
    assert(compilerError)
  }
  test("Can create a Matrix from a Seq of row Tuple arguments") {
    val mat = Matrix(((1, 2), (3, 4), (5.0, 6.0)))
    assert(mat.rows == 3 && mat.columns == 2)
  }
  test("Seq[Tuple] Matrix with jagged rows should throw IllegalArgumentException") {
    val compilerError = try {
      val mat = Matrix((1, 2), (3, 4, 5)) // compiler error
      printf("%s\n", mat)
      false // fail if exception not thrown 
    } catch {
      case _ =>
        true // as expected
    } 
    assert(compilerError)
  }
}
