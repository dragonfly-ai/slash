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

import narr.*
import slash.*
import slash.vector.*
import slash.vectorf.*

class ConversionTest extends munit.FunSuite {

  test(" VecF -> Vec -> VecF ") {

    val vf0: VecF[3] = VecF.random[3](Float.MinValue, Float.MaxValue)
    val v0: Vec[3] = vf0.toVec
    val vf1: VecF[3] = v0.toVecF

    assertEquals(vf0.x, vf1.x)
    assertEquals(vf0.y, vf1.y)
    assertEquals(vf0.z, vf1.z)

  }

}
