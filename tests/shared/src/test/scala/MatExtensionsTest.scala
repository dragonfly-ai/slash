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
    val tupA = (((1.1, 2.1), (3.1, 4.1), (5.1, 6.1))) // single Tuple[Tuple] arg
    val tupB =  ((1.1, 2.1), (3.1, 4.1), (5.1, 6.1))  // repeating Tuple args
    val m01: Mat[3, 2] = Mat(tupA)
    val m02: Mat[3, 2] = Mat(tupB)
    assert(m01.strictEquals(m02))
  }
  test("Mat with NaN fields unequal even if NaNs are aligned") {
    val m01: Mat[1, 2] = Mat(3, Double.NaN)
    val m02: Mat[1, 2] = Mat(3, Double.NaN)
    assert(!m01.strictEquals(m02))
  }
  test("Tuples with non-number fields throw IllegalArgumentException") {
    val compilerError = try {
      Mat[3,2](((1, 2), (3, 4), ("", false)))
      false // fail if exception not thrown 
    } catch {
      case _ =>
        true // as expected
    }
    assert(compilerError)
  }
  test("Tuples with jagged rows should throw IllegalArgumentException") {
    val compilerError = try {
      val mat = Mat.fromTuples[2,3]((1, 2), (3, 4, 5))
      printf("%s\n", mat)// compiler error
      false // fail if exception not thrown 
    } catch {
      case _ =>
        true // as expected
    } 
    assert(compilerError)
  }
  test("Can create a Mat from a Seq of row Tuple arguments") {
    val m0 = Mat.fromTuples[3,2]((1, 2), (3, 4), (5, 6))
    assert(m0.rows == 3 && m0.columns == 2)
  }
  test("Can create a Mat from a Seq of row Tuples of type Long") {
    val m0L = Mat.fromTuples[3,2]((1L, 2L), (3L, 4L), (5L, 6L))
    assert(m0L.rows == 3 && m0L.columns == 2)
  }
  test("Can create a Mat from a Seq of row Tuples of type Float") {
    val m0f = Mat.fromTuples[2,3]((1f, 2f, 3f), (4f, 5f, 6f))
    assert(m0f.rows == 2 && m0f.columns == 3, s"m0f.rows[${m0f.rows}] != 2 or m0f.columns[${m0f.columns}] != 3")
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

  test("Mat from one tuple row") {
    val m1 = Mat[1,2](
      (10754.536093848204,  26063.070416174756),
    )
    val m2:Mat[1,2] = Mat(
      (10754.536093848204,  26063.070416174756),
    )
    val m3:Mat[1,2] = Mat[1,2](
      (10754.536093848204,  26063.070416174756),
    )
    assert(m1.strictEquals(m2))
    assert(m2.strictEquals(m3))

    val dims1 = (m1.rows, m1.columns)
    val dims2 = (m2.rows, m2.columns)
    val dims3 = (m3.rows, m3.columns)
    assert(dims1 == dims2 && dims2 == dims3)
  }

  test("Correctly handle a bare TupleN[Int,Int,...]") {
    // arg is a Tuple3[Int,Int,Int] rather than a Tuple1[Tuple3[Int,Int,Int]]
    val m0 = Mat[1,3]( (1,2,3) )
    assert(m0.rows == 1 && m0.columns == 3, s"error handling special case [$m0]")

    // by contrast, this is a Tuple2[Tuple3[Int,Int,Int]]
    val m1 = Mat.fromTuples[2,3](
      (1,2,3),
      (5,6,7)
    )
    assert(m1.rows == 2 && m1.columns == 3, s"error handling special case [$m1]")
  }

  test("Correctly handle a bare TupleN[Double,Double,...]") {
    // arg is a Tuple3[Int,Int,Int] rather than a Tuple1[Tuple3[Int,Int,Int]]
    val m0 = Mat[1,3]( (1.0, 2.0, 3.0) )
    assert(m0.rows == 1 && m0.columns == 3, s"error handling special case [$m0]")

    // by contrast, this is a Tuple2[Tuple3[Int,Int,Int]]
    val m1 = Mat.fromTuples[2,3](
      (1.0,2.0,3.0),
      (5.0,6.0,7.0)
    )
    assert(m1.rows == 2 && m1.columns == 3, s"error handling special case [$m1]")
  }

  test("single row Mat from a single tuple"){
    val m1 = Mat[1,4]((1,2,3,4))
    assert(m1.rows == 1 && m1.columns == 4, s"m1.rows[${m1.rows}] != 1 || m1.columns[${m1.columns}] != 4")
  }
  test("single row Mat from numeric args representing a single row"){
    val m2 = Mat[1,4](1,2,3,4)
    assert(m2.rows == 1 && m2.columns == 4, s"m2.rows[${m2.rows}] != 1 || m2.columns[${m2.columns}] != 4")
  }
  test("single row Mat from a single tuple"){
    val m3 = Mat[1,4]((1,2,3,4))
    //printf("m3.rows[%s] should == 1, m3.columns[%s] should == 4\n", m3.rows, m3.columns)
    assert(m3.rows == 1 && m3.columns == 4)
  }

  test("Mat with various dimension declarations equate"){
    val m1:Mat[3,3] = Mat.fromTuples[3,3]((
      (  4187.029699912913,  10754.536093848204,  26063.070416174756),
      (-25818.603476607503,   5330.547028842389, -19754.31142736372 ),
      (-14414.095464827511,  17658.70671706525 ,  28269.642346736226)
    ))
    val m2 = Mat.fromTuples[3,3]((
      (  4187.029699912913,  10754.536093848204,  26063.070416174756),
      (-25818.603476607503,   5330.547028842389, -19754.31142736372 ),
      (-14414.095464827511,  17658.70671706525 ,  28269.642346736226)
    ))
    val m3:Mat[3,3] = Mat.fromTuples[3,3]((
      (  4187.029699912913,  10754.536093848204,  26063.070416174756),
      (-25818.603476607503,   5330.547028842389, -19754.31142736372 ),
      (-14414.095464827511,  17658.70671706525 ,  28269.642346736226)
    ))
    assert(m1.strictEquals(m2), s"m1[$m1] != m2[$m2]")
    assert(m2.strictEquals(m3), s"m2[$m2] != m3[$m3]")
  }
}
