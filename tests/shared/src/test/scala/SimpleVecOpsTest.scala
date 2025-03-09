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
import narr.*



class SimpleVecOpsTest extends munit.FunSuite {

  test("vector addition") {
    val v1 = Vec[2](1.5, 2.5)
    val v2 = Vec[2](2.5, 3.5)
    val vResult = Vec[2](4.0, 6.0)
    assertVecEquals(v1 + v2, vResult)
  }
  test("vector subtraction") {
    val v1 = Vec[2](1.5, 2.5)
    val v2 = Vec[2](2.5, 3.5)
    val vResult = Vec[2](-1.0, -1.0)
    assertVecEquals(v1 - v2, vResult)
  }

  test("scalar addition") {
    val v1 = Vec[3](1.5, 2.5, 3.5)
    val vResult = Vec[3](2.5, 3.5, 4.5)
    assertVecEquals(v1 + 1, vResult)
  }

  test("scalar negation") {
    val v1 = Vec[3](1.5, 2.5, 3.5)
    val vResult = Vec[3](0.5, 1.5, 2.5)
    assertVecEquals(v1 - 1.0, vResult)
  }

  test("scalar multiplication") {
    val v1 = Vec[3](1.5, 2.5, 3.5)
    val vResult = Vec[3](3.0, 5.0, 7.0)
    assertVecEquals(v1 * 2.0, vResult)
  }
  test("right multiplication by scala expression") {
    val v1 = Vec[3](1.5, 2.5, 3.5)
    val vResult = Vec[3](3.0, 5.0, 7.0)
    assertVecEquals(v1 * 4.0/2.0, vResult)
  }
  test("scalar left multiplication") {
    val v1 = Vec[3](1.5, 2.5, 3.5)
    val vResult = Vec[3](3.0, 5.0, 7.0)
    assertVecEquals(2.0 * v1, vResult)
  }

  test("scalar division") {
    val v1 = Vec[3](1.5, 2.5, 3.5)
    val vResult = Vec[3](0.75, 1.25, 1.75)
    assertVecEquals(v1 / 2.0, vResult)
  }

  test("clampedMin") {
    val v1 = Vec[3](1.5, 2.5, 3.5)
    val vResult = Vec[3](2.0, 2.5, 3.5)
    assertVecEquals(v1.clampedMin(2.0), vResult)
  }

  test("clampedMAX") {
    val v1 = Vec[3](1.5, 2.5, 3.5)
    val vResult = Vec[3](1.5, 2.0, 2.0)
    assertVecEquals(v1.clampedMAX(2.0), vResult)
  }
}

