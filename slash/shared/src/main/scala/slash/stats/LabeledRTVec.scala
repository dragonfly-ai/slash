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

package slash.stats

import slash.vector.runtime.*

trait LabeledRTVec {
  def label: Double
  def vector: RTVec
  def y:Double = label
  def x: RTVec = vector
}

case class SimpleLabeledRTVector(override val label: Double, override val vector: RTVec) extends LabeledRTVec {
  override def toString: String = s"SimpleLabeledVec[${vector.dimension}]($label, ${vector.render()})"

}

case class ContextualLabeledRTVector[T](override val label: Double, override val vector:RTVec, context:T) extends LabeledRTVec {
  override def toString: String = s"ContextualLabeledRTVector[${vector.dimension}]($label, ${vector.render()})"
}