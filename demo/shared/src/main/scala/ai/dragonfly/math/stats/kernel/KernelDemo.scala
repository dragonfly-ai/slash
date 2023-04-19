package ai.dragonfly.math.stats.kernel

/**
 * Created by clifton on 5/16/15.
 */

import ai.dragonfly.math.*
import Constant.{`√(2π)`, π}
import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.stats.DenseHistogramOfContinuousDistribution
import ai.dragonfly.math.stats.probability.distributions.Gaussian
import ai.dragonfly.math.vector.*
import narr.*

import scala.collection.mutable

object KernelDemo extends Demonstration {
  override def demo():Unit = {
    val exclusionRadius:Double = 12.0

    var t: Double = 0.0
    val step:Double = 0.1
    val totalSteps:Double = exclusionRadius / step

    type N = 1

    val gk:GaussianKernel[N] = GaussianKernel[N](exclusionRadius, new Gaussian(0.0, 16.0))
    val ek:EpanechnikovKernel[N] = EpanechnikovKernel[N](exclusionRadius)
    val uk:UniformKernel[N] = UniformKernel[N](exclusionRadius)
    val dk:DiscreteKernel[N] = DiscreteKernel[N](
      exclusionRadius,
      NArray.tabulate[Double](squareInPlace(totalSteps).toInt)((i:Int) => {
        val t2:Double = squareInPlace(i * step)
        0.5 * (gk.weight(t2) + ek.weight(t2))
      })
    )


    val gkh = new DenseHistogramOfContinuousDistribution(10, 0.0, 12.0)
    val ekh = new DenseHistogramOfContinuousDistribution(10, 0.0, 12.0)
    val ukh = new DenseHistogramOfContinuousDistribution(10, 0.0, 12.0)
    val dkh = new DenseHistogramOfContinuousDistribution(10, 0.0, 12.0)

    while (t < exclusionRadius) {

      val t2: Double = squareInPlace(t)

      gkh( t, gk.weight(t2) )
      ekh( t, ek.weight(t2) )
      ukh( t, uk.weight(t2) )
      dkh( t, dk.weight(t2) )

      t = t + 0.1
    }

    println("Gaussian Kernel ")
    println(gkh.toString)
    println("Epanechnikov Kernel ")
    println(ekh.toString)
    println("Uniform Kernel ")
    println(ukh.toString)
    println("Discrete Kernel ")
    println(dkh.toString)
  }

  override def name: String = "Kernel"
}
