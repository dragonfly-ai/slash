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

package ai.dragonfly.math.vector

import ai.dragonfly.math.*
import narr.*

/**
 * Created by clifton on 1/10/17.
 */

object Vector2 {

  import Vector.*

  def rotateAllDegrees(vectors:NArray[Vector[2]], degrees: Double): NArray[Vector[2]] = {
    rotateAll(vectors, degreesToRadians(degrees))
  }

  def rotateAll(vectors:NArray[Vector[2]], radians: Double): NArray[Vector[2]] = {
    val cos:Double = Math.cos(radians)
    val sin:Double = Math.sin(radians)
    var v2 = 0
    while (v2 < vectors.length) {
      vectors(v2).rotate(cos, sin)
      v2 = v2 + 1
    }
    vectors
  }

  extension (thisVector: Vector[2]) {
//    inline def apply(index: Int): Double = thisVector(index)
//
//    inline def update(index: Int, value: Double): Unit = thisVector(index) = value
    inline def x: Double = thisVector(0)
    inline def y: Double = thisVector(1)

    inline def rotate(cosTheta:Double, sinTheata:Double):Vector[2] = {
      val x1 = thisVector.x * cosTheta - thisVector.y * sinTheata
      thisVector(1) = thisVector.x * sinTheata + thisVector.y * cosTheta
      thisVector(0) = x1
      thisVector
    }
    inline def rotate(radians: Double): Vector[2] = rotate( Math.cos(radians), Math.sin(radians) )
    inline def rotateDegrees(degrees: Double): Vector[2] = rotate(degreesToRadians(degrees))
    inline def pseudoCross(v: Vector[2]): Double = x * v.y + y * v.x

    /**
     * Compute the signed angle between two vectors.
     * @param v the second vector to compare this vector to.
     * @return the signed angle in radians
     */
    inline def angleFrom(v: Vector[2]): Double = {
      //Math.acos( (thisVector dot v) / (thisVector.norm * v.norm) )  // unsigned method
      Math.atan2(thisVector.pseudoCross(v), thisVector dot v)
    }

    def show: String = s"《²↗〉${x}ᵢ ${y}ⱼ〉" // ₂⃗ ²↗ ↗²
  }

}
