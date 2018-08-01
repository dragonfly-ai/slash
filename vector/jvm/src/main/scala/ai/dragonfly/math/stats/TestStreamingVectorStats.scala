package ai.dragonfly.math.stats

import ai.dragonfly.math.vector.VectorN

/**
 * Created by clifton on 1/10/17.
 */

object TestStreamingVectorStats extends App {

  val svs = new StreamingVectorStats(4)

  for (i <- 0 until 10000) svs(VectorN.random(4, 1000))

  println(svs)

}