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

package slash.vectorf

import slash.*
import slash.vectorf.vectorf.VecF

case class VectorFBounds[N <: Int](min: VecF[N], MAX: VecF[N])(using ValueOf[N]) {
  lazy val dimension:Int = valueOf[N]
  lazy val center:VecF[N] = (min + MAX) / 2.0
  lazy val boundingRadius:Float = Math.max(min.magnitude, MAX.magnitude)

  def contains(v: VecF[N]):Boolean = {
    var o:Boolean = true
    var i:Int = 0; while(o && i < min.dimension) {
      o = min(i) <= v(i) && v(i) <= MAX(i)
      i += 1
    }
    o
  }

  def minEuclidianDistanceSquaredTo(v: VecF[N]): Double = {
    var sumOfSquares:Double = 0
    var i: Int = 0
    while (i < min.dimension) {
      sumOfSquares = sumOfSquares + squareInPlace(Math.max(0.0, Math.max(min(i) - v(i), v(i) - MAX(i))))
      i += 1
    }
    sumOfSquares
  }

  def minEuclidianDistanceTo(v: VecF[N]): Double = Math.sqrt(minEuclidianDistanceSquaredTo(v))

  import slash.Random.*

  def random(r: scala.util.Random = Random.defaultRandom):VecF[N] = r.betweenF[N](min, MAX)


  /**
   * Determines if this VectorFBounds intersects another VectorFBounds.
   * @param that another VectorFBounds.
   * @return true if this VectorFBounds intersects that VectorFBounds.
   */
  def intersects(that: VectorFBounds[N]): Boolean = {
    var overlaps: Boolean = true
    var i: Int = 0
    while (overlaps && i < min.dimension) {
      overlaps = min(i) <= that.MAX(i) && that.min(i) <= MAX(i)
      i += 1
    }
    overlaps
  }

  /**
   * Determines if the given sphere intersects this VectorFBounds.
   * @param v the center vector of the sphere.
   * @param radiusSquared the radius of the sphere.
   * @return true if this VectorFBounds intersects the sphere.
   */
  def intersectsSphere(v: VecF[N], radiusSquared: Float): Boolean = {
    var distSquared = 0.0

    var i: Int = 0
    while (i < min.dimension) {
      if (v(i) < min(i)) distSquared += squareInPlace(v(i) - min(i))
      else if (v(i) > MAX(i)) distSquared += squareInPlace(v(i) - MAX(i))
      i = i + 1
    }
    
    distSquared <= radiusSquared
  }

  def copy:VectorFBounds[N] = VectorFBounds[N](min.copy, MAX.copy)

  override def toString: String = s"VectorFBounds[$dimension](\n  ${min.show},\n  ${MAX.show}\n)"

  // Grok 2.0 suggests adding these methods:

  /*
maxDistanceTo(Vector point)
intersects(VectorFBounds other)
intersectsLine(Vector start, Vector end)
union(VectorFBounds other)
intersection(VectorFBounds other)
   */

}
