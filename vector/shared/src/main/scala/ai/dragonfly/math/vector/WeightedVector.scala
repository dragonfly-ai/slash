package ai.dragonfly.math.vector

import ai.dragonfly.math.example.Demonstrable

object WeightedVector extends Demonstrable {

  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val wv0 = WeightedVector[Vector3](Vector3(1.1, 2.5, 0.1), 0.5)
    sb.append(s"\tWeightedVector: $wv0\n")
    sb.append(s"\tWeightedVector.weighted: ${wv0.weighted}\n")
    sb.append(s"\tWeightedVector.weight: ${wv0.weight}\n")
    sb.append(s"\tWeightedVector.addWeight(0.25): ${wv0.addWeight(0.25)}\n")
    sb.append(s"\tWeightedVector: $wv0\n")
    sb.append(s"\tWeightedVector.weighted: ${wv0.weighted}\n")
    sb.append(s"\tWeightedVector.weight: ${wv0.weight}\n")
  }

  override def name: String = "WeightedVector"
}

case class WeightedVector[V <: Vector[V]](unweighted: V, private var w: Double = 0.0) {
  def weight:Double = w
  def addWeight(w1: Double): WeightedVector[V] = {
    w = w + w1
    this
  }
  def weighted: V = unweighted * weight
}