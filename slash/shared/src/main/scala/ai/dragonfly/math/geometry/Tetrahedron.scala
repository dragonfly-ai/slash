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

package ai.dragonfly.math.geometry

import ai.dragonfly.math.interval.*
import ai.dragonfly.math.interval.Interval.*
import ai.dragonfly.math.stats.probability.distributions.Sampleable
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.vector.Vec.*
import narr.*

object Tetrahedron {

  val `1/6`:Double = 1.0 / 6.0
  def apply(v1: Vec[3], v2: Vec[3], v3: Vec[3], v4: Vec[3]):Tetrahedron = Tetrahedron(NArray[Vec[3]](v1, v2, v3, v4))

}

case class Tetrahedron(vertices:NArray[Vec[3]]) extends Sampleable[Vec[3]] {

  import Tetrahedron.*

  inline def v1:Vec[3] = vertices(0)
  inline def v2:Vec[3] = vertices(1)
  inline def v3:Vec[3] = vertices(2)
  inline def v4:Vec[3] = vertices(3)

  private def `v1-v4` = v1 - v4
  private def `v2-v4` = v2 - v4
  private def `v3-v4` = v3 - v4

  // Formula for the Volume of a tetrahedron taken from:
  // https://en.wikipedia.org/wiki/Tetrahedron#Volume

  def volume:Double = `1/6` * Math.abs(`v1-v4` dot (`v2-v4` ⨯ `v3-v4`))

  override def random(r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom): Vec[3] = {

    var w1 = r.nextDouble()
    var w2 = r.nextDouble()
    var w3 = r.nextDouble()

    if (w1 > w2) {
      val t = w1
      w1 = w2
      w2 = t
    }

    if (w2 > w3) {
      val t = w2
      w2 = w3
      w3 = t
    }

    if (w1 > w2) {
      val t = w1
      w1 = w2
      w2 = t
    }

    (`v1-v4` * w1).add(`v2-v4` * (w2 - w1)).add(`v3-v4` * (w3 - w2)).add(v4)
  }


}
