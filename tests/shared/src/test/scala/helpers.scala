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
import slash.matrix.Matrix


inline def assertVecEquals[N <: Int](inline v1: Vec[N], inline v2: Vec[N])(implicit loc: munit.Location): Unit = {
  var i: Int = 0;
  while (i < v1.dimension) {
    munit.Assertions.assertEquals(v1(i), v2(i))
    i += 1
  }
}

inline def assertMatrixEquals[M <: Int, N <: Int](inline m1: Matrix[M, N], inline m2: Matrix[M, N])(implicit loc: munit.Location): Unit = {
  var i: Int = 0;
  while (i < m1.MxN) {
    munit.Assertions.assertEquals(m1.values(i), m2.values(i))
    i += 1
  }
}