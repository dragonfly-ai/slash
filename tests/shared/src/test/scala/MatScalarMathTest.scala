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
import scala.language.implicitConversions

class MatScalarMathTest extends munit.FunSuite {

  test("verify Mat scalar add is commutative") {
    val a0 = Mat[2,6](2,  3,  4,  5,  6,  7, 8,  9, 10, 11, 12, 13)
    val a1 = a0 + 2
    val a2 = 2 + a0
    val a3 = a0 + 2
    val a4 = 2.0 + a0
    assert(a1.strictEquals(a2) && a1.strictEquals(a3) && a1.strictEquals(a4))
  }

  test("verify Mat scalar subtract is commutative") {
    val a0 = Mat[2,6](2,  3,  4,  5,  6,  7, 8,  9, 10, 11, 12, 13)
    val a1 = a0 - 2
    val a2 = -2 + a0
    val a3 = a0 - 2.0
    val a4 = -2.0 + a0
    assert(a1.strictEquals(a2) && a1.strictEquals(a3) && a1.strictEquals(a4))
  }

  test("verify Mat scalar multiply is commutative") {
    val m0 = Mat[2,6](2,  3,  4,  5,  6,  7, 8,  9, 10, 11, 12, 13)
    val m1 = m0 * 3
    val m2 = 3.0 * m0
    val m3 = m0 * 3
    val m4 = 3 * m0
    assert(m1.strictEquals(m2) && m1.strictEquals(m3) && m1.strictEquals(m4))
  }

  test("verify Mat - scalar") {
    val matrix = Mat[2,6]( 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
    val expect = Mat[2,6](-1, 0, 1, 2, 3, 4, 5, 6,  7,  8,  9, 10)
    val matmod = matrix - 3
    // printf("%s\n", matmod)
    assert(matmod.strictEquals(expect))
  }

  test("verify Mat -= scalar") {
    val matvar = Mat[2,6]( 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
    val expect = Mat[2,6](-1, 0, 1, 2, 3, 4, 5, 6,  7,  8,  9, 10)
    matvar -= 3
    // printf("\nmatrix:\n%s\n", matvar)
    // printf("expect:\n%s\n", expect)
    assert(matvar.strictEquals(expect))
  }

  test("verify scalar - Mat") {
    val matrix = Mat[2,6]( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    val expect = Mat[2,6](-2,-1, 0, 1, 2, 3, 4, 5, 6,  7,  8,  9)
    val matmod = -3 + matrix
    // printf("\nmatrix:\n%s\n", matrix)
    // printf("matmod:\n%s\n", matmod)
    // printf("expect:\n%s\n", expect)
    assert(matmod.strictEquals(expect))
  }

  test("verify Mat += scalar") {
    val matvar = Mat[2,6](1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12)
    val expect = Mat[2,6](4, 5, 6, 7, 8, 9,10,11,12,13,14,15)
    matvar += 3
    // printf("\nmatrix:\n%s\n", matvar)
    // printf("expect:\n%s\n", expect)
    assert(matvar.strictEquals(expect))
  }
  test("verify scalar * Mat") {
    val matrix = Mat[2,6](1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12)
    val expect = Mat[2,6](2, 4, 6, 8,10,12,14,16,18,20,22,24)
    val matmod = 2 * matrix
    // printf("\nmatrix:\n%s\n", matmod)
    // printf("expect:\n%s\n", expect)
    assert(matmod.strictEquals(expect))
  }
  test("verify Mat *= scalar") {
    val matvar = Mat[2,6](1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12)
    val expect = Mat[2,6](2, 4, 6, 8,10,12,14,16,18,20,22,24)
    matvar *= 2
    // printf("\nmatrix:\n%s\n", matvar)
    // printf("expect:\n%s\n", expect)
    assert(matvar.strictEquals(expect))
  }
}
