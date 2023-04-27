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

object Vec3 {

  import Vec.*

  extension (thisVector: Vec[3]) {
    inline def x: Double = thisVector(0)
    inline def y: Double = thisVector(1)
    inline def z: Double = thisVector(2)

    inline def ⨯(thatVector: Vec[3]): Vec[3] = cross(thatVector)

    inline def cross(thatVector: Vec[3]): Vec[3] = Vec[3](
      y * thatVector.z - z * thatVector.y, // u2*v3 - u3*v2,
      z * thatVector.x - x * thatVector.z, // u3*v1 - u1*v3,
      x * thatVector.y - y * thatVector.x // u1*v2 - u2*v1
    )

    def show: String = s"《³↗〉${x}ᵢ ${y}ⱼ ${z}ₖ〉"
  }
}
