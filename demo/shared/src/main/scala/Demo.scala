import ai.dragonfly.democrossy.{Demonstration, NativeConsole, XApp}
import ai.dragonfly.math.geometry.TetrahedronDemo

/**
 * Created by clifton on 1/9/17.
 */

object Demo extends XApp(NativeConsole(style = "padding: 8px; overflow: scroll;")) {

  val allDemos: Array[Demonstration] = Array[Demonstration](
//    ai.dragonfly.math.vector.SparseVectorDemo,
    ai.dragonfly.math.unicode.UnicodeDemo,
    ai.dragonfly.math.vector.Vec2Demo,
    ai.dragonfly.math.vector.Vec3Demo,
    ai.dragonfly.math.vector.Vec4Demo,
    ai.dragonfly.math.vector.VecNDemo,
    ai.dragonfly.math.vector.WeightedVecDemo,
    ai.dragonfly.math.geometry.TetrahedronDemo,
    ai.dragonfly.math.spatial.OctreeDemo,
    ai.dragonfly.math.stats.kernel.KernelDemo,
    ai.dragonfly.math.GammaDemo,
    ai.dragonfly.math.FactorialDemo,
    // add Interval demo
    ai.dragonfly.math.stats.probability.distributions.stream.StreamingVectorStatsDemo,
    ai.dragonfly.math.stats.probability.distributions.GaussianDemo.demo,
    ai.dragonfly.math.stats.probability.distributions.PoissonDemo.demo,
    ai.dragonfly.math.stats.probability.distributions.LogNormalDemo.demo,
    ai.dragonfly.math.stats.probability.distributions.PERTDemo.demo,
    ai.dragonfly.math.stats.probability.distributions.BetaDemo.demo2param,
    ai.dragonfly.math.stats.probability.distributions.BetaDemo.demo4param,
    ai.dragonfly.math.stats.probability.distributions.BinomialDemo.demo,
    ai.dragonfly.math.stats.probability.distributions.UniformDemo.demo,
    ai.dragonfly.math.stats.probability.distributions.DiscreteUniformDemo.demo,
    ai.dragonfly.math.stats.probability.distributions.stream.GaussianDemo.demo,
    ai.dragonfly.math.stats.probability.distributions.stream.PoissonDemo.demo,
    ai.dragonfly.math.stats.probability.distributions.stream.LogNormalDemo.demo,
    ai.dragonfly.math.stats.probability.distributions.stream.PERTDemo,
    ai.dragonfly.math.stats.probability.distributions.stream.BetaDemo.demo,
    ai.dragonfly.math.stats.probability.distributions.stream.BinomialDemo.fixedBinomialDemo,
    ai.dragonfly.math.stats.probability.distributions.stream.BinomialDemo.openBinomialDemo
  )

  def main(args: Array[String]): Unit = {
    for (d <- allDemos) d.demonstrate
  }

}
