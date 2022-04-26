package ai.dragonfly.math.stats.kernel

/**
 * Created by clifton on 5/16/15.
 */

import ai.dragonfly.math.*
import Constant.{`√(2π)`, π}
import ai.dragonfly.math.stats.DenseHistogramOfContinuousDistribution
import example.*
import ai.dragonfly.math.stats.probability.distributions.Gaussian
import ai.dragonfly.math.vector.*

import scala.collection.mutable

object Kernel extends Demonstrable {
  override def demo(using sb:mutable.StringBuilder = new mutable.StringBuilder()):mutable.StringBuilder = {
    val exclusionRadius:Double = 12.0

    var t: Double = 0.0
    val step:Double = 0.1
    val totalSteps:Double = exclusionRadius / step

    val gk:GaussianKernel = GaussianKernel(exclusionRadius, new Gaussian(0.0, 16.0))
    val ek:EpanechnikovKernel = EpanechnikovKernel(exclusionRadius)
    val uk:UniformKernel = UniformKernel(exclusionRadius)
    val dk:DiscreteKernel = DiscreteKernel(
      exclusionRadius,
      Array.tabulate[Double](squareInPlace(totalSteps).toInt)((i:Int) => {
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

    sb.append("Gaussian Kernel ").append(gkh.toString)
    sb.append("Epanechnikov Kernel ").append(ekh.toString)
    sb.append("Uniform Kernel ").append(ukh.toString)
    sb.append("Discrete Kernel ").append(dkh.toString)

    sb
  }

  override def name: String = "Kernel"
}

trait Kernel {
  val exclusionRadius: Double
  lazy val exclusionRadiusSquared:Double = exclusionRadius * exclusionRadius

  def weight(magnitudeSquared: Double): Double
  def weight(v: Vector): Double
  def weight(v1: Vector, v2: Vector): Double = weight(v1 - v2)
//  def scaledWeight(v1: VectorN, v2: VectorN): Double = weight(v1, v2) * v1.getFrequency * v2.getFrequency

  def distance(v: Vector): Double = v.norm
  def distance(v1: Vector, v2: Vector): Double = (v1 - v2).norm

  lazy val discretize: DiscreteKernel = DiscreteKernel(this)
}


case class GaussianKernel(exclusionRadius: Double, gaussian:Gaussian) extends Kernel {

  def weight(v: Vector): Double = weight(v.normSquared)

  def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else gaussian.p2(magnitudeSquared)
  }
}


case class EpanechnikovKernel(exclusionRadius: Double) extends Kernel {
  def weight(v: Vector): Double = weight(v.normSquared)

  def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else 0.75 - 0.75 * (magnitudeSquared / exclusionRadiusSquared)
  }

  //  def naiveWeight(v: VectorectorN): Double = naiveWeight(v.magnitudeSquared)
  //
  //  def naiveWeight(magnitudeSquared: Double): Double = {
  //    val l = Math.sqrt(magnitudeSquared) / radius
  //    if (l > 1) 0.0
  //    else 0.75 - 0.75 * (l * l)
  //  }
}


case class UniformKernel(exclusionRadius: Double) extends Kernel {
  def weight(v: Vector): Double = weight(v.normSquared)

  def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else 1.0 / (π * exclusionRadiusSquared)
  }
}


object DiscreteKernel {
  def apply(k: Kernel): DiscreteKernel = {
    val maxMagSquared: Int = Math.ceil(k.exclusionRadiusSquared).toInt
    val weights = new Array[Double](maxMagSquared + 1)
    for (d <- 0 to maxMagSquared) weights(d) = k.weight(d)
    DiscreteKernel(k.exclusionRadius, weights)
  }
}


case class DiscreteKernel(exclusionRadius: Double, weights: Array[Double]) extends Kernel {

  lazy val totalWeights:Double = {
    var total = 0.0
    val r = exclusionRadius.toInt
    for (dy <- -r to r; dx <- -r to r) total += weight(squareInPlace(dx) + squareInPlace(dy))
    total
  }

  override def weight(v: Vector): Double = weight(v.normSquared)

  def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else weights(magnitudeSquared.toInt)
  }

}
