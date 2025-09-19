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

import narr.*
import slash.*
import slash.vectorf.vectorf.VecF

/**
 * Created by clifton on 1/10/17.
 */

object VecF2 {

  def rotateAllDegrees(vectors:NArray[VecF[2]], degrees: Double): NArray[VecF[2]] = {
    rotateAll(vectors, degreesToRadians(degrees))
  }

  def rotateAll(vectors:NArray[VecF[2]], radians: Double): NArray[VecF[2]] = {
    val cos:Double = Math.cos(radians)
    val sin:Double = Math.sin(radians)
    var vi = 0
    while (vi < vectors.length) {
      val v2:VecF[2] = vectors(vi)
      v2.rotate(cos, sin)
      vi = vi + 1
    }
    vectors
  }


}
