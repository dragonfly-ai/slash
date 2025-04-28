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
import slash.interval.*

class VectorBoundsTest extends munit.FunSuite {

  val N:Int = 1000
  val r:scala.util.Random = defaultRandom
  val interval: Interval[Double] = `[]`[Double](
    r.between(Short.MinValue.toDouble, 0.0),
    r.between(Double.MinPositiveValue, Short.MaxValue.toDouble)
  )


  test(" VectorBounds.contains ") {
    val vs = VectorSpace(2 + r.nextInt(42))
    import vs.n

    val bounds:VectorBounds[vs.N] = VectorBounds(
      vs.random(interval.min, 0.0),
      vs.random(Double.MinPositiveValue, interval.MAX)
    )

    // inside
    var i = 0
    while (i < N) {
      assert(bounds.contains(bounds.random()))
      i = i + 1
    }

    // outside
    i = 0
    while (i < N) {
      val vt = bounds.random()
      val j:Int = r.nextInt(vt.dimension)
      val delta:Double = r.between(0.0000001, 1.0)
      vt(j) = if (r.nextDouble() >= 0.5) bounds.min(j) - delta else bounds.MAX(j) + delta
      if (bounds.contains(vt)) {
        println(s"${vt.show}\n$bounds")
      }
      assert(!bounds.contains(vt))
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

  test(" VectorBounds.intersects ") {
    val vs = VectorSpace(2 + r.nextInt(4))
    import vs.n
    val bounds:VectorBounds[vs.N] = VectorBounds(
      vs.random(interval.min, 0.0),
      vs.random(Double.MinPositiveValue, interval.MAX)
    )

    def randomFromInside:VectorBounds[vs.N] = {
      val min = bounds.random()
      val MAX = bounds.random()
      val c = min.copy
      var vi = 0
      while (vi < c.dimension) {
        min(vi) = Math.min(c(vi), MAX(vi))
        MAX(vi) = Math.max(c(vi), MAX(vi))
        vi = vi + 1
      }
      VectorBounds[vs.N](min, MAX)
    }

    def randomEnclosing: VectorBounds[vs.N] = VectorBounds[vs.N](
      bounds.min + vs.random(interval.min, 0.0),
      bounds.MAX + vs.random(Double.MinPositiveValue, interval.MAX)
    )

    def randomGreaterThan: VectorBounds[vs.N] = {
      val min = bounds.MAX + vs.random(Double.MinPositiveValue, interval.MAX)
      VectorBounds[vs.N](
        min,
        min + vs.random(Double.MinPositiveValue, interval.MAX)
      )
    }

    def randomLessThan: VectorBounds[vs.N] = {
      val MAX = bounds.min + vs.random(interval.min, 0.0)
      VectorBounds[vs.N](
        MAX + vs.random(interval.min, 0.0),
        MAX
      )
    }

    def randomOffByOne: VectorBounds[vs.N] = {
      val out = bounds.copy

      val i = r.nextInt(vs.dimension)
      out.min(i) = bounds.MAX(i) + 42 + (42 + r.nextDouble())
      out.MAX(i) = out.min(i) + 42 + (42 + r.nextDouble())

      out
    }

    var i = 0
    while (i < N) {
      // fully encloses:
      assert(bounds.intersects(randomFromInside))
      // fully enclosed by:
      assert(bounds.intersects(randomEnclosing))
      // fully greater than:
      assert(!bounds.intersects(randomGreaterThan))
      // fully less than:
      assert(!bounds.intersects(randomLessThan))
      // off by one dimension:
      assert(!bounds.intersects(randomOffByOne))
      i = i + 1
    }
  }
}
