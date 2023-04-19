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

trait LabeledVector[N <: Int] {
  def label: Double
  def vector: Vector[N]
  def `f(x)`:Double = label
  def y:Double = label
  def x: Vector[N] = vector
}

case class SimpleLabeledVector[N <: Int](override val label: Double, override val vector: Vector[N]) extends LabeledVector[N] {
  override def toString: String = s"SimpleLabeledVector($label, ${vector.render()})"

}

case class ContextualLabeledVector[N <: Int, T](override val label: Double, override val vector:Vector[N], context:T) extends LabeledVector[N] {
  override def toString: String = s"ContextualLabeledVector($label, ${vector.render()})"
}