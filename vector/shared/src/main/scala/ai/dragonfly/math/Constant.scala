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

package ai.dragonfly.math

object Constant {

  // Literal
  val π: Double = Math.PI // Pi
  val `√(2π)`:Double = Math.sqrt(2.0*π)
  val e: Double = Math.E  // Euler's Number
  val `𝜑`: Double = 1.46557123187676802665 // Golden

  /**
   * The largest possible Long value such that Double can contiguously represent every Long value in the range: [0L, xL]
   */
  val MaxContiguousLong:Long = 9007199254740992L

  // Computed
  lazy val log2:Double = Math.log10(2.0) // base 10 logarithm of 2
  lazy val ln2:Double = Math.log(2.0)    // natural logarithm of 2
  lazy val `√2`:Double = Math.sqrt(2.0)  // square root of 2

}