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

package slash

import slash.vector.*
import scala.compiletime.ops.any.==
import scala.compiletime.ops.boolean.||

package object matrix {
  extension[M <: Int, N <: Int] (thisMatrix: Matrix[M, N])(using ValueOf[M], ValueOf[N], (M == 1 || N == 1) =:= true) {
    def asVector: Vec[thisMatrix.MN] = thisMatrix.rowPackedArray.asInstanceOf[Vec[thisMatrix.MN]]
  }
}
