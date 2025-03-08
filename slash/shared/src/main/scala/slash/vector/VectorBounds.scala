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

package slash.vector

import Vec.*
import slash.*

case class VectorBounds[N <: Int](min: Vec[N], MAX: Vec[N])(using ValueOf[N]) {

  lazy val center:Vec[N] = (min + MAX) / 2.0
  lazy val boundingRadius:Double = Math.max(min.magnitude, MAX.magnitude)

  def contains(v: Vec[N]):Boolean = {
    var o:Boolean = true
    var i:Int = 0; while(o && i < min.dimension) {
      o = min(i) <= v(i) && v(i) <= MAX(i)
      i += 1
    }
    o
  }

  def minEuclidianDistanceSquaredTo(v: Vec[N]): Double = {
    var sumOfSquares:Double = 0
    var i: Int = 0
    while (i < min.dimension) {
      sumOfSquares = sumOfSquares + squareInPlace(Math.max(0.0, Math.max(min(i) - v(i), v(i) - MAX(i))))
      i += 1
    }
    sumOfSquares
  }

  def minEuclidianDistanceTo(v: Vec[N]): Double = Math.sqrt(minEuclidianDistanceSquaredTo(v))

  import slash.Random.*

  def random(r: scala.util.Random = Random.defaultRandom):Vec[N] = r.between[N](min, MAX)

  // Grok 2.0 suggests adding these methods:

  /*
maxDistanceTo(Vector point)
intersects(VectorBounds other)
intersectsLine(Vector start, Vector end)
union(VectorBounds other)
intersection(VectorBounds other)
   */

}
