package ai.dragonfly.math.stats

import ai.dragonfly.math.vector.*

case class LabeledVector(label: Double, vector: Vector) {
  def `f(x)`:Double = label
  def y:Double = label
  def x:Vector = vector
}
