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

package slash.vector.runtime

import slash.*

case class RTVectorBounds(min: RTVec, MAX: RTVec) {

  require(min.dimension == MAX.dimension, s"min and MAX vectors of RTVectorBounds must have equal dimensions.")

  lazy val dimension:Int = min.dimension
  lazy val center:RTVec = (min + MAX) / 2.0
  lazy val boundingRadius:Double = Math.max(min.magnitude, MAX.magnitude)

  def contains(v: RTVec):Boolean = {
    dimensionCheck(dimension, v.dimension)
    var o:Boolean = true
    var i:Int = 0
    while(o && i < min.dimension) {
      o = min(i) <= v(i) && v(i) <= MAX(i)
      i += 1
    }
    o
  }

  def minEuclidianDistanceSquaredTo(v: RTVec): Double = {
    dimensionCheck(dimension, v.dimension)
    var sumOfSquares:Double = 0
    var i: Int = 0
    while (i < min.dimension) {
      sumOfSquares = sumOfSquares + squareInPlace(Math.max(0.0, Math.max(min(i) - v(i), v(i) - MAX(i))))
      i += 1
    }
    sumOfSquares
  }

  def minEuclidianDistanceTo(v: RTVec): Double = Math.sqrt(minEuclidianDistanceSquaredTo(v))

  import slash.Random.*

  def random(r: scala.util.Random = Random.defaultRandom):RTVec = r.between(min, MAX)

  /**
   * Determines if this RTVectorBounds intersects another RTVectorBounds.
   * @param that another RTVectorBounds.
   * @return true if this RTVectorBounds intersects that RTVectorBounds.
   */
  def intersects(that: RTVectorBounds): Boolean = {
    var overlaps: Boolean = true
    var i: Int = 0
    while (overlaps && i < min.dimension) {
      overlaps = min(i) <= that.MAX(i) && that.min(i) <= MAX(i)
      i += 1
    }
    overlaps
  }

  /**
   * Determines if the given sphere intersects this RTVectorBounds.
   * @param v the center vector of the sphere.
   * @param radiusSquared the radius of the sphere.
   * @return true if this RTVectorBounds intersects the sphere.
   */
  def intersectsSphere(v: RTVec, radiusSquared: Double): Boolean = {
    var distSquared = 0.0

    var i: Int = 0
    while (i < min.dimension) {
      if (v(i) < min(i)) distSquared += squareInPlace(v(i) - min(i))
      else if (v(i) > MAX(i)) distSquared += squareInPlace(v(i) - MAX(i))
      i = i + 1
    }

    distSquared <= radiusSquared
  }

  def copy:RTVectorBounds = RTVectorBounds(min.copy, MAX.copy)

  override def toString: String = s"RTVectorBounds(\n dimension = $dimension\n  ${min.show},\n  ${MAX.show}\n)"


}