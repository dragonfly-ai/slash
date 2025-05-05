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

class MatConcatenateTest extends munit.FunSuite {
  test("can concatenate rows of 2 matrices") {
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
    val allRows = Mat.concatenateRows(mat1,mat2)
    assert(allRows.strictEquals(expected))
  }
  test("can concatenate columns of 2 matrices") {
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
    val allRows = Mat.concatenateColumns(mat1,mat2)
    assert(allRows.strictEquals(expected))
  }
}
