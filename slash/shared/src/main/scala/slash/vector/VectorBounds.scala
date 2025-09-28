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

import slash.*
import Vec.*

case class VectorBounds[N <: Int](min: Vec[N], MAX: Vec[N])(using ValueOf[N]) {
  lazy val dimension:Int = valueOf[N]
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


  /**
   * Determines if this VectorBounds intersects another VectorBounds.
   * @param that another VectorBounds.
   * @return true if this VectorBounds intersects that VectorBounds.
   */
  def intersects(that: VectorBounds[N]): Boolean = {
    var overlaps: Boolean = true
    var i: Int = 0
    while (overlaps && i < min.dimension) {
      overlaps = min(i) <= that.MAX(i) && that.min(i) <= MAX(i)
      i += 1
    }
    overlaps
  }

  /**
   * Determines if the given sphere intersects this VectorBounds.
   * @param v the center vector of the sphere.
   * @param radiusSquared the radius of the sphere.
   * @return true if this VectorBounds intersects the sphere.
   */
  def intersectsSphere(v: Vec[N], radiusSquared: Double): Boolean = {
    var distSquared = 0.0

    var i: Int = 0
    while (i < min.dimension) {
      if (v(i) < min(i)) distSquared += squareInPlace(v(i) - min(i))
      else if (v(i) > MAX(i)) distSquared += squareInPlace(v(i) - MAX(i))
      i = i + 1
    }
    
    distSquared <= radiusSquared
  }

  def copy:VectorBounds[N] = VectorBounds[N](min.copy, MAX.copy)

  override def toString: String = s"VectorBounds[$dimension](\n  ${min.show},\n  ${MAX.show}\n)"

  // Grok 2.0 suggests adding these methods:

  /*
maxDistanceTo(Vector point)
intersects(VectorBounds other)
intersectsLine(Vector start, Vector end)
union(VectorBounds other)
intersection(VectorBounds other)
   */

}
