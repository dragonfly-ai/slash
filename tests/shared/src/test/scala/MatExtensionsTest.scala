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
import slash.matrix.Mat.*

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

  test("Mat can be initialized from Tuple literal") {
    val tup3x2 = (((1, 2), (3, 4), (5, 6)))
    val m01: Mat[3, 2] = Mat(tup3x2)
    val m02 = Mat(((1, 2), (3, 4), (5, 6)))
    assert(m01.strictEquals(m02))
  }
  test("Mat equality for Tuples with mixed number types") {
    // values selected to avoid roundoff errors
    val m01: Mat[2, 2] = Mat(((1, 2L), (2.0, 4f))) // Int, Long, Double, Float
    val m02: Mat[2, 2] = Mat(((1L, 2.0), (2f, 4))) // Long, Double, Float, Int
    assert(m01.strictEquals(m02))
  }
  test("Same values but unequal Dimensions should not equate") {
    val mat3x2 = Mat[3,2](1, 2, 3, 4, 5, 6)
    val mat2x3 = Mat[2,3](1, 2, 3, 4, 5, 6)
    assert(!mat3x2.strictEquals(mat2x3))
  }
  test("Single or repeating Tuple parameters should be equivalent") {
    val tup1 = (((1, 2), (3, 4), (5, 6))) // single Tuple[Tuple] arg
    val tup2 =  ((1, 2), (3, 4), (5, 6))  // repeating Tuple args
    val m01: Mat[3, 2] = Mat(tup1)
    val m02: Mat[3, 2] = Mat(tup2)
    assert(m01.strictEquals(m02))
  }
  test("Mat with NaN fields unequal even if NaNs are aligned") {
    val m01: Mat[1, 2] = Mat(3, Double.NaN)
    val m02: Mat[1, 2] = Mat(3, Double.NaN)
    assert(!m01.strictEquals(m02))
  }
  test("Tuples with non-number fields throw IllegalArgumentException") {
    val tup1 = (((1, 2), (3, 4), ("", false))) // last row is (NaN, NaN)
    val compilerError = try {
      val mat = Mat(tup1)
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
      val mat = Mat(((1, 2), (3, 4, 5))) // compiler error
      printf("%s\n", mat)
      false // fail if exception not thrown 
    } catch {
      case _ =>
        true // as expected
    } 
    assert(compilerError)
  }
  test("Can create a Mat from a Seq of row Tuple arguments") {
    val mat = Mat((1, 2), (3, 4), (5, 6))
    assert(mat.rows == 3 && mat.columns == 2)
  }
  test("Seq[Tuple] Mat with jagged rows should throw IllegalArgumentException") {
    val compilerError = try {
      val mat = Mat((1, 2), (3, 4, 5)) // compiler error
      printf("%s\n", mat)
      false // fail if exception not thrown 
    } catch {
      case t =>
        true // as expected
    } 
    assert(compilerError)
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


}
