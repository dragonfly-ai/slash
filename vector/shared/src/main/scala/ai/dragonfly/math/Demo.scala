package ai.dragonfly.math

import ai.dragonfly.math.util.Demonstrable
import ai.dragonfly.math.vector.*

/**
 * Created by clifton on 1/9/17.
 */

object Demo {
  val allDemos: Array[Demonstrable] = Array[Demonstrable](
    Vector2,
    Vector3,
    Vector4,
    VectorN,
    WeightedVector,
    ai.dragonfly.math.stats.Gaussian.demo,
    ai.dragonfly.math.stats.Poisson.demo,
    ai.dragonfly.math.stats.LogNormal.demo,
    ai.dragonfly.math.stats.Beta.demo2param,
    ai.dragonfly.math.stats.Beta.demo4param,
    ai.dragonfly.math.stats.stream.Gaussian.demo,
    ai.dragonfly.math.stats.stream.Poisson.demo,
    ai.dragonfly.math.stats.stream.LogNormal.demo,
    ai.dragonfly.math.stats.stream.Beta.demo,
    ai.dragonfly.math.stats.stream.StreamingVectorStats,
    ai.dragonfly.math.stats.kernel.Kernel,
    ai.dragonfly.math.stats.geometry.Tetrahedron,
    TestGamma,
    ai.dragonfly.math.util.Factorial.demo
  )

  lazy val consolidateDemoOutput: String = {
    implicit val sb: StringBuilder = new StringBuilder()
    for (d <- allDemos) {
      sb.append(s"\n\n/* Begin ${d.name} Demonstration */\n")
      sb.append(d.demo())
      sb.append(s"// End ${d.name} Demonstration.\n\n")
    }
    sb.toString()
  }



  def main(args: Array[String]): Unit = {
    println(consolidateDemoOutput)
  }

}
