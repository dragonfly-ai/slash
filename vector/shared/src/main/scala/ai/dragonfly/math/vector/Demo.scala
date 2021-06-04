package ai.dragonfly.math.vector

import ai.dragonfly.math.util.Demonstrable

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

/**
 * Created by clifton on 1/9/17.
 */

@JSExportTopLevel("Demo") @JSExportAll
object Demo {
  val allDemos:Array[Demonstrable] = Array[Demonstrable](
    V2,
    V3,
    V4,
    VN,
    WeightedVector,
    ai.dragonfly.math.stats.Gaussian,
    ai.dragonfly.math.stats.Poisson,
    ai.dragonfly.math.stats.stream.Gaussian,
    ai.dragonfly.math.stats.stream.Poisson,
    ai.dragonfly.math.stats.stream.StreamingVectorStats,
    ai.dragonfly.math.stats.kernel.Kernel,
    ai.dragonfly.math.stats.geometry.Tetrahedron
  )

  lazy val consolidateDemoOutput:String = {
    implicit val sb:StringBuilder = new StringBuilder()
    for (d <- allDemos){
      sb.append(s"\n\n/* Begin ${d.name} Demonstration */\n{")
      sb.append(d.demo())
      sb.append(s"} // End ${d.name} Demonstration.")
    }
    sb.toString()
  }

  def main(args: Array[String]): Unit = {
    println(consolidateDemoOutput)
  }

}