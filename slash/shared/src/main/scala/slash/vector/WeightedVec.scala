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

package slash.vector

import Vec.*

case class WeightedVec[N <: Int](unweighted: Vec[N], private var w: Double = 0.0) {
  def weight:Double = w
  def addWeight(w1: Double): WeightedVec[N] = {
    w = w + w1
    this
  }
  def weighted: Vec[N] = unweighted * weight

  override def toString: String = s"WeightedVector($weight, ${unweighted.render()})"
}