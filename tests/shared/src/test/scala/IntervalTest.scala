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
import slash.interval.Interval.*

import scala.reflect.ClassTag

class IntervalTest extends munit.FunSuite {

  case class IntervalTest[DOMAIN:ClassTag](i:Interval[DOMAIN]) {
    def test():Unit = {
      assert(i.contains(nextDown(i.min)))
//      assert(!i.contains(-100))
//      assert(!i.contains(-100))
    }

  }

  test(" Testing Intervals: [], [), (], () ") {
    val i0 = `[]`[Double](-10.0, 10.0)
    assert(i0.contains(0))
    assert(!i0.contains(-100))
    assert(!i0.contains(-100))
  }
}
