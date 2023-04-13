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

import ai.dragonfly.math.squareInPlace
import narr.*

/**
 * Created by clifton on 1/10/17.
 */

object Vector3 extends VectorCompanion[Vector3] {

  inline given dimension: Int = 3

  override inline def validDimension(dimension: Int): Boolean = dimension == 3

  override def apply(values:NArray[Double]): Vector3 = new Vector3(dimensionCheck(values, dimension))

  def apply(x: Double, y: Double, z: Double):Vector3 = apply(NArray[Double](x, y, z))

  def random(maxNorm:Double = 1.0): Vector3 = Vector3(maxNorm * Math.random(), maxNorm * Math.random(), maxNorm * Math.random())

}


case class Vector3 private (override val values:NArray[Double]) extends Vector {

  type VEC = Vector3

  inline def x:Double = values(0)
  inline def y:Double = values(1)
  inline def z:Double = values(2)

  inline def ⨯ (v: Vector3): Vector3 = cross(v)

  inline def cross(v: Vector3): Vector3 = Vector3(
    y * v.z - z * v.y, // u2*v3 - u3*v2,
    z * v.x - x * v.z, // u3*v1 - u1*v3,
    x * v.y - y * v.x  // u1*v2 - u2*v1
  )

  override inline def copy(): VEC = Vector3(x, y, z)

  override def toString: String = s"《³↗〉${x}ᵢ ${y}ⱼ ${z}ₖ〉"

}