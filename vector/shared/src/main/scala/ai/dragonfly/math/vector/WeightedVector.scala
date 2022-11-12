package ai.dragonfly.math.vector


case class WeightedVector[V <: ai.dragonfly.math.vector.Vector](unweighted: V, private var w: Double = 0.0) {
  def weight:Double = w
  def addWeight(w1: Double): WeightedVector[V] = {
    w = w + w1
    this
  }
  def weighted: V = (unweighted * weight).asInstanceOf[V]
}