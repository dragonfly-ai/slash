package ai.dragonfly.math.vector

import ai.dragonfly.democrossy.Demonstration

import Vector.*
import Vector3.*

object WeightedVectorDemo extends Demonstration {

  override def demo():Unit = {
    val wv0 = WeightedVector[3](Vector[3](1.1, 2.5, 0.1), 0.5)
    println(s"\tWeightedVector: $wv0\n")
    println(s"\tWeightedVector.weighted: ${wv0.weighted}\n")
    println(s"\tWeightedVector.weight: ${wv0.weight}\n")
    println(s"\tWeightedVector.addWeight(0.25): ${wv0.addWeight(0.25)}\n")
    println(s"\tWeightedVector: $wv0\n")
    println(s"\tWeightedVector.weighted: ${wv0.weighted}\n")
    println(s"\tWeightedVector.weight: ${wv0.weight}\n")
  }

  override def name: String = "WeightedVector"
}
