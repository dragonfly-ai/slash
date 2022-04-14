package ai.dragonfly.math.stats

import ai.dragonfly.math.vector.*

trait LabeledVector {
  def label: Double
  def vector: Vector
  def `f(x)`:Double = label
  def y:Double = label
  def x:Vector = vector
}

case class SimpleLabeledVector(override val label: Double, override val vector: Vector) extends LabeledVector {

}

case class ContextualLabeledVector[T](override val label: Double, override val vector:Vector, context:T) extends LabeledVector {

}