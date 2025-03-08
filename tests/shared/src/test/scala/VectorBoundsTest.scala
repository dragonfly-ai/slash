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
import slash.Random.*
import slash.vector.*
import slash.interval.ContinuousInterval

class VectorBoundsTest extends munit.FunSuite {

  val N:Int = 1000
  val r:scala.util.Random = defaultRandom

  test(" VectorBounds.contains ") {
    val vs = VectorSpace(2 + r.nextInt(42))
    import vs.n

    val interval: ContinuousInterval = ContinuousInterval(
      slash.interval.Interval.CLOSED,
      r.between(Int.MinValue.toDouble, 0.0),
      r.between(Double.MinPositiveValue, Int.MaxValue.toDouble)
    )

    val bounds:VectorBounds[vs.N] = VectorBounds(
      vs.random(interval.min, 0.0),
      vs.random(Double.MinPositiveValue, interval.MAX)
    )

    // inside
    var i = 0; while (i < N) {
      assert(bounds.contains(bounds.random()))
      i = i + 1
    }

    // outside
    i = 0
    while (i < N) {
      val v = bounds.random()
      v.normalize()
      assert(!bounds.contains(v * (bounds.boundingRadius + r.between(0.00000001, 1.0))))
      i = i + 1
    }
  }

  test(" VectorBounds.minEuclidianDistanceSquaredTo") {
    val bounds: VectorBounds[3] = VectorBounds(Vec[3](-50.0, -50.0, -50.0), Vec[3](50.0, 50.0, 50.0))

    // all sides:
    assert(1.0 == bounds.minEuclidianDistanceSquaredTo(Vec[3](-51.0, 0.0, 0.0)))
    assert(1.0 == bounds.minEuclidianDistanceSquaredTo(Vec[3](0.0, -51.0, 0.0)))
    assert(1.0 == bounds.minEuclidianDistanceSquaredTo(Vec[3](0.0, 0.0, -51.0)))

    assert(1.0 == bounds.minEuclidianDistanceSquaredTo(Vec[3](51.0, 0.0, 0.0)))
    assert(1.0 == bounds.minEuclidianDistanceSquaredTo(Vec[3](0.0, 51.0, 0.0)))
    assert(1.0 == bounds.minEuclidianDistanceSquaredTo(Vec[3](0.0, 0.0, 51.0)))

    // all corners:
    assert(3.0 == bounds.minEuclidianDistanceSquaredTo(Vec[3](51.0, 51.0, 51.0)))
    assert(3.0 == bounds.minEuclidianDistanceSquaredTo(Vec[3](51.0, 51.0, -51.0)))
    assert(3.0 == bounds.minEuclidianDistanceSquaredTo(Vec[3](51.0, -51.0, 51.0)))
    assert(3.0 == bounds.minEuclidianDistanceSquaredTo(Vec[3](51.0, -51.0, -51.0)))
    assert(3.0 == bounds.minEuclidianDistanceSquaredTo(Vec[3](-51.0, 51.0, 51.0)))
    assert(3.0 == bounds.minEuclidianDistanceSquaredTo(Vec[3](-51.0, 51.0, -51.0)))
    assert(3.0 == bounds.minEuclidianDistanceSquaredTo(Vec[3](-51.0, -51.0, 51.0)))
    assert(3.0 == bounds.minEuclidianDistanceSquaredTo(Vec[3](-51.0, -51.0, -51.0)))

  }
}
