package ai.dragonfly.math.examples

import ai.dragonfly.math.*
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
    ai.dragonfly.math.stats.kernel.Kernel,
    ai.dragonfly.math.stats.geometry.Tetrahedron,
    TestGamma,
    Factorial.demo,
    ai.dragonfly.math.stats.probability.distributions.stream.StreamingVectorStats,
    ai.dragonfly.math.stats.probability.distributions.Gaussian.demo,
    ai.dragonfly.math.stats.probability.distributions.Poisson.demo,
    ai.dragonfly.math.stats.probability.distributions.LogNormal.demo,
    ai.dragonfly.math.stats.probability.distributions.PERT.demo,
    ai.dragonfly.math.stats.probability.distributions.Beta.demo2param,
    ai.dragonfly.math.stats.probability.distributions.Beta.demo4param,
    ai.dragonfly.math.stats.probability.distributions.Binomial.demo,
    ai.dragonfly.math.stats.probability.distributions.Uniform.demo,
    ai.dragonfly.math.stats.probability.distributions.DiscreteUniform.demo,
    ai.dragonfly.math.stats.probability.distributions.stream.Gaussian.demo,
    ai.dragonfly.math.stats.probability.distributions.stream.Poisson.demo,
    ai.dragonfly.math.stats.probability.distributions.stream.LogNormal.demo,
    ai.dragonfly.math.stats.probability.distributions.stream.PERT.demo,
    ai.dragonfly.math.stats.probability.distributions.stream.Beta.demo,
    ai.dragonfly.math.stats.probability.distributions.stream.Binomial.demo,
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
