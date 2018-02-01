package ai.dragonfly.math.stats

import ai.dragonfly.math.vector.Vector3

object TestHistograms extends App {

  val dh = new DiscreteHistogram[Vector3]
  for (i <- 0 until 100) {
    dh.adjust(
      Vector3(Math.round(10 * Math.random()), Math.round(10 * Math.random()), Math.round(10 * Math.random())),
      (10 * Math.random()).toInt
    )
  }
  println(dh.hist)
  println()

  val doh = new DiscreteOrderedHistogram[Integer]
  for (i <- 0 until 100) {
    doh.adjust(
      (10 * Math.random()).toInt,
      (10 * Math.random()).toInt
    )
  }
  println(doh.hist)

}