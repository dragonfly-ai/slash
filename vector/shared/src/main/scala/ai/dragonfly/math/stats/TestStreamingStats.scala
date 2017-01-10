package ai.dragonfly.math.stats

import ai.dragonfly.math.vector._

import ai.dragonfly.math.stats.kernel.GaussianKernel

import scala.scalajs.js.annotation.JSExport

/**
 * Created by clifton on 1/10/17.
 */

@JSExport("test.TestStreamingStats")
object TestStreamingStats {
  @JSExport def test0(): Unit = {
    val k = GaussianKernel(5)

    val vectorStats = new StreamingVectorStats(5)
    for (i <- 0 until 100) {
      val rVector = VectorN.random(5, 2)
      vectorStats(rVector, k.weight(rVector))
    }
    println(vectorStats)

    println("Average: " + vectorStats.average())
    println("Average: " + vectorStats.variance())
    println("Average: " + vectorStats.standardDeviation())
  }

}
