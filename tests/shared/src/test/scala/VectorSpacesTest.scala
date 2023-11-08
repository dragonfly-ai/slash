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
import slash.Constant.π

import slash.Random.defaultRandom as r

class VectorSpacesTest extends munit.FunSuite {
  test(" testing kitchen sink ") {
    var runtimeDimension:Int = r.nextInt(42)
    runtimeDimension += 1

    val vs = VectorSpace(runtimeDimension)

    assertEquals(vs.dimension, runtimeDimension)

    val kitchenSink: Vec[vs.N] = vs.fill(π) + ((vs.ones + vs.zeros) * 16) - vs.tabulate( (i:Int) => i / π )

    // this exercises VectorSpace.{ones, zeros, tabulate, and fill}
    var i:Int = 0; while (i < vs.dimension) {
      assertEquals(kitchenSink(i), π + 1*16 - (i / π))
      i += 1
    }

  }

  test(" testing VectorSpace.fromTuple ") {

    val vs = VectorSpace(2 + r.nextInt(3))

    vs.fromTuple(
      vs.dimension match {
        case 2 => (r.nextDouble(), r.nextDouble())
        case 3 => (r.nextDouble(), r.nextDouble(), r.nextDouble())
        case 4 => (r.nextDouble(), r.nextDouble(), r.nextDouble(), r.nextDouble())
        case 5 => (r.nextDouble(), r.nextDouble(), r.nextDouble(), r.nextDouble(), r.nextDouble())
      }
    )

  }
}
