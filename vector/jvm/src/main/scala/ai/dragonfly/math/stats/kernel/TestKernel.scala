package ai.dragonfly.math.stats.kernel

/**
 * Created by clifton on 1/9/17.
 */

object TestKernel extends App {
  val r = 32
  val dk = DiscreteKernel(GaussianKernel(r))

  println(dk.totalWeights)

  var count = 0
  for (dy <- -r to r; dx <- -r to r) count = count + 1
  println(count)
}
