package ai.dragonfly.math.stats.kernel

/**
 * Created by clifton on 5/16/15.
 */

import ai.dragonfly.math.*
import Constant.{`√(2π)`, π}
import ai.dragonfly.math.stats.DenseHistogramOfContinuousDistribution
import ai.dragonfly.math.stats.probability.distributions.Gaussian
import ai.dragonfly.math.vector.*

import scala.collection.mutable


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
