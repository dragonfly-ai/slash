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

import narr.NArray
import slash.interval.*

class ArithmeticProgressionTest extends munit.FunSuite {

  test("arithmeticProgression produces expected result"){
    val a = arithmeticProgression(-1, +1, 5)
    val expect = NArray[Double](-1.0, -0.5, 0.0, 0.5, 1.0)
    val adump = a.mkString(",")
    val edump = expect.mkString(",")
    // printf("a[%s]\n", adump)
    // printf("e[%s]\n", edump)
    assert(adump == edump)
  }
  test("verify arithmeticProgression default length parameter"){
    val a: NArray[Double] = arithmeticProgression(-1, +1)
    val expect = arithmeticProgression(-1, +1, 100)
    val adump = a.mkString(",")
    val edump = expect.mkString(",")
    // printf("a[%s]\n", adump.take(50))
    // printf("e[%s]\n", edump.take(50))
    assert(adump == edump)
  }

}
