package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.Random.*
import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.vector._
import narr.*

/**
 * Created by clifton on 1/10/17.
 */


object StreamingVectorStatsDemo extends Demonstration {
  override def demo():Unit = {
    val svs = new StreamingVectorStats(4)
    for (i <- 0 until 10000) svs(defaultRandom.nextVector4(1000))
    println(svs)
  }

  override def name: String = "StreamingVectorStats"
}
