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

package ai.dragonfly.math.stats

import ai.dragonfly.math.vector.*

trait LabeledVec[N <: Int] {
  def label: Double
  def vector: Vec[N]
  def `f(x)`:Double = label
  def y:Double = label
  def x: Vec[N] = vector
}

case class SimpleLabeledVector[N <: Int](override val label: Double, override val vector: Vec[N]) extends LabeledVec[N] {
  override def toString: String = s"SimpleLabeledVec[${vector.dimension}]($label, ${vector.render()})"

}

case class ContextualLabeledVector[N <: Int, T](override val label: Double, override val vector:Vec[N], context:T) extends LabeledVec[N] {
  override def toString: String = s"ContextualLabeledVec[${vector.dimension}]($label, ${vector.render()})"
}