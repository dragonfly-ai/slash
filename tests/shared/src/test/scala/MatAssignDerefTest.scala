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

class MatAssignDerefTest extends munit.FunSuite {

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

}
