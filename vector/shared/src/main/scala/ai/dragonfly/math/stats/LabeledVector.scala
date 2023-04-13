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

trait LabeledVector[V <: ai.dragonfly.math.vector.Vector] {
  def label: Double
  def vector: V
  def `f(x)`:Double = label
  def y:Double = label
  def x:V = vector
}

case class SimpleLabeledVector[V <: ai.dragonfly.math.vector.Vector](override val label: Double, override val vector: V) extends LabeledVector[V] {

}

case class ContextualLabeledVector[V <: ai.dragonfly.math.vector.Vector, T](override val label: Double, override val vector:V, context:T) extends LabeledVector[V] {

}