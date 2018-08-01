package ai.dragonfly.math.stats

import scala.scalajs.js.annotation.JSExportTopLevel

/**
 * Created by clifton on 1/10/17.
 */

@JSExportTopLevel("test.TestStreamingStats")
object TestStreamingStats extends App {
  val ss = new StreamingStats()

  for (i <- 0 until 10000) {
    ss(Math.random() * 1000)
  }

  println(ss)
}