package ai.dragonfly.math.vector

import ai.dragonfly.democrossy.Demonstration

import Vec.*

object WeightedVecDemo extends Demonstration {

  override def demo():Unit = {
    val wv0 = WeightedVec[3](Vec[3](1.1, 2.5, 0.1), 0.5)
    println(s"\tWeightedVec: $wv0\n")
    println(s"\tWeightedVec.weighted: ${wv0.weighted}\n")
    println(s"\tWeightedVec.weight: ${wv0.weight}\n")
    println(s"\tWeightedVec.addWeight(0.25): ${wv0.addWeight(0.25)}\n")
    println(s"\tWeightedVec: $wv0\n")
    println(s"\tWeightedVec.weighted: ${wv0.weighted}\n")
    println(s"\tWeightedVec.weight: ${wv0.weight}\n")
  }

  override def name: String = "WeightedVec[3]"
}
