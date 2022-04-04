package ai.dragonfly.math.example

import ai.dragonfly.math.*
import ai.dragonfly.math.stats.geometry.Tetrahedron
import vector.*

/**
 * Created by clifton on 1/9/17.
 */

object Demo {

  val allDemos: Array[Demonstrable] = Array[Demonstrable](
    Tetrahedron,
    Vector2,
    Vector3,
    Vector4,
    VectorN,
    WeightedVector,
    stats.kernel.Kernel,
    stats.geometry.Tetrahedron,
    TestGamma,
    Factorial.demo,
    // add Interval demo
    visualization.ConsoleImage,
    visualization.Chart,
    stats.probability.distributions.stream.StreamingVectorStats,
    stats.probability.distributions.Gaussian.demo,
    stats.probability.distributions.Poisson.demo,
    stats.probability.distributions.LogNormal.demo,
    stats.probability.distributions.PERT.demo,
    stats.probability.distributions.Beta.demo2param,
    stats.probability.distributions.Beta.demo4param,
    stats.probability.distributions.Binomial.demo,
    stats.probability.distributions.Uniform.demo,
    stats.probability.distributions.DiscreteUniform.demo,
    stats.probability.distributions.stream.Gaussian.demo,
    stats.probability.distributions.stream.Poisson.demo,
    stats.probability.distributions.stream.LogNormal.demo,
    stats.probability.distributions.stream.PERT.demo,
    stats.probability.distributions.stream.Beta.demo,
    stats.probability.distributions.stream.Binomial.fixedBinomialDemo,
    stats.probability.distributions.stream.Binomial.openBinomialDemo
  )
  import Console.{GREEN, RED, RESET, YELLOW, UNDERLINED, RED_B}

  lazy val consolidateDemoOutput: String = {
    implicit val sb: StringBuilder = new StringBuilder()
    for (d <- allDemos) {
      sb.append(s"\n\n/* ${RESET}${GREEN}Begin ${d.name} Demonstration${RESET}*/\n")
      sb.append(s"${d.demo()}")
      sb.append(s"/* ${RESET}${RED}End ${d.name} Demonstration${RESET} */\n\n")
    }
    sb.toString()
  }

  def main(args: Array[String]): Unit = {
    println(s"$RESET$GREEN$consolidateDemoOutput$RESET")
  }

}
