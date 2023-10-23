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

package ai.dragonfly.math.interval

import Interval.*


object Domain {

  val ℕ_Int: Domain[Int] = Domain[Int](`[]`(1, Int.MaxValue))
  val ℕ_Long: Domain[Long] = Domain[Long](`[]`(1L, Long.MaxValue))

  val `ℕ₀_Int`: Domain[Int] = Domain[Int](`[]`(0, Int.MaxValue))
  val `ℕ₀_Long`: Domain[Long] = Domain[Long](`[]`(0, Long.MaxValue))

  val ℤ_Int: Domain[Int] = Domain[Int](`[]`(Int.MinValue, Int.MaxValue))
  val ℤ_Long: Domain[Long] = Domain[Long](`[]`(Long.MinValue, Long.MaxValue))

  val ℝ_Float: Domain[Float] = Domain[Float](`[]`(Float.MinValue, Float.MaxValue))
  val ℝ_Double: Domain[Double] = Domain[Double](`[]`(Double.MinValue, Double.MaxValue))

  val `ℝ+_Float`: Domain[Float] = Domain[Float](`(]`(0f, Float.MaxValue))
  val `ℝ+_Double`: Domain[Double] = Domain[Double](`(]`(0.0, Double.MaxValue))

  val `ℝ[0,1]_Float`: Domain[Float] = Domain[Float](`[]`(0f, 1f))
  val `ℝ[0,1]_Double`: Domain[Double] = Domain[Double](`[]`(0.0, 1.0))

  val `ℝ[-1,1]_Float`: Domain[Float] = Domain[Float](`[]`(-1f, 1f))
  val `ℝ[-1,1]_Double`: Domain[Double] = Domain[Double](`[]`(-1.0, 1.0))


}

case class Domain[DOMAIN](interval:Interval[DOMAIN])(using `#`: Numeric[DOMAIN]) {
  val `0`:DOMAIN = `#`.zero // Additive Identity
  val `1`:DOMAIN = `#`.one // Multiplicative Identity
  def zero:DOMAIN = `0`
  def one:DOMAIN = `1`
  def min:DOMAIN = interval.min
  def MAX:DOMAIN = interval.MAX
}
