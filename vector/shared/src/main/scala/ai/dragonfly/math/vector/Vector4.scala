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

import scala.language.postfixOps
import ai.dragonfly.math.squareInPlace
import narr.*

object Vector4 extends VectorCompanion[Vector4] {

  inline given dimension: Int = 4

  override inline def validDimension(dimension: Int): Boolean = dimension == 4

  override def apply(values:NArray[Double]): Vector4 = new Vector4(dimensionCheck(values, dimension))

  def apply(x: Double, y: Double, z: Double, w: Double):Vector4 = new Vector4(NArray[Double](x, y, z, w))

  def random(maxNorm:Double = 1.0): Vector4 = Vector4(maxNorm * Math.random(), maxNorm * Math.random(), maxNorm * Math.random(), maxNorm * Math.random())

}

case class Vector4 private (override val values: NArray[Double]) extends Vector {

  type VEC = Vector4

  inline def x:Double = values(0)
  inline def y:Double = values(1)
  inline def z:Double = values(2)
  inline def w:Double = values(3)

  override inline def copy(): VEC = Vector4(x, y, z, w)

  override def toString: String = s"《⁴↗〉${x}ᵢ ${y}ⱼ ${z}ₖ ${w}ₗ〉"

}
