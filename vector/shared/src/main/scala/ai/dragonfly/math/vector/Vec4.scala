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

object Vec4 {

  import Vec.*

  extension (thisVector: Vec[4]) {
    inline def x: Double = thisVector(0)
    inline def y: Double = thisVector(1)
    inline def z: Double = thisVector(2)
    inline def w: Double = thisVector(3)
    def show: String = s"《⁴↗〉${x}ᵢ ${y}ⱼ ${z}ₖ ${w}ₗ〉"
  }
}
