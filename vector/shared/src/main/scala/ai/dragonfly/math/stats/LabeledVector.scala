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