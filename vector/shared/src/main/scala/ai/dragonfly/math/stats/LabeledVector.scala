package ai.dragonfly.math.stats

import ai.dragonfly.math.vector.*

trait LabeledVector[V <: VectorData with Vector[V]] {
  def label: Double
  def vector: V
  def `f(x)`:Double = label
  def y:Double = label
  def x:V = vector
}

case class SimpleLabeledVector[V <: VectorData with Vector[V]](override val label: Double, override val vector: V) extends LabeledVector[V] {

}

case class ContextualLabeledVector[T, V <: VectorData with Vector[V]](override val label: Double, override val vector:V, context:T) extends LabeledVector[V] {

}