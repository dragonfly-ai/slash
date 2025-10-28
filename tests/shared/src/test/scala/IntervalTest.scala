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

import slash.*
import slash.interval.*

import scala.reflect.ClassTag

class IntervalTest extends munit.FunSuite {

  case class IntervalTester[DOMAIN](i:Interval[DOMAIN]) {
    def test():Unit = {
      if (i.leftClosed) assert(i.contains(i.min))
      else assert(!i.contains(i.min))

      if (i.rightClosed) assert(i.contains(i.MAX))
      else assert(!i.contains(i.MAX))

      assert(!i.contains(nextDown(i.min)))
      assert(i.contains(nextUp(i.min)))
      assert(i.contains(nextDown(i.MAX)))
      assert(!i.contains(nextUp(i.MAX)))

      var i0:Int = 0; while(i0 < 100) {
        assert(i.contains(i.random()))
        i0 += 1
      }
    }

  }

  val intervalTests = Array[IntervalTester[Double]](
    IntervalTester(`[]`[Double](-9.3542, 11.0006)),
    IntervalTester(`[)`[Double](-9.3542, 11.0006)),
    IntervalTester(`(]`[Double](-9.3542, 11.0006)),
    IntervalTester(`()`[Double](-9.3542, 11.0006))
  )

  test(" Testing Intervals: [], [), (], () ") {
    var i:Int = 0; while (i < intervalTests.length) {
      intervalTests(i).test()
      i += 1
    }
  }
}
