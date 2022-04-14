package ai.dragonfly.math.stats.kernel

/**
 * Created by clifton on 5/16/15.
 */

import ai.dragonfly.math.*
import Constant.π
import example.*
import ai.dragonfly.math.vector.*

object Kernel extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val r = 32
    val dk = DiscreteKernel(GaussianKernel(r))

    sb.append(dk.totalWeights)

    var count = 0
    for (dy <- -r to r; dx <- -r to r) count = count + 1
    sb.append(count).append("\n")
  }

  override def name: String = "Kernel"
}

trait Kernel[V <: VectorData with Vector[V]] {
  val radius: Double
  lazy val radiusSquared:Double = radius * radius

  def weight(magnitudeSquared: Double): Double
  def weight(v: V): Double
  def weight(v1: V, v2: V): Double = weight(v1 - v2)
//  def scaledWeight(v1: VectorN, v2: VectorN): Double = weight(v1, v2) * v1.getFrequency * v2.getFrequency

  def distance(v: V): Double = v.norm
  def distance(v1: V, v2: V): Double = (v1 - v2).norm

  lazy val discretize: DiscreteKernel[V] = DiscreteKernel[V](this)
}

object GaussianKernel {
  def apply[V <: VectorData with Vector[V]](radius: Double): GaussianKernel[V] = {
    val sigma: Double = radius / 3.0
    val denominator: Double = 2.0 * (sigma * sigma)
    GaussianKernel[V](radius, sigma, denominator, 1.0 / Math.sqrt(π * denominator))
  }
}


case class GaussianKernel[V <: VectorData with Vector[V]](radius: Double, sigma: Double, denominator: Double, c: Double) extends Kernel[V] {
  def weight(v: V): Double = weight(v.normSquared)

  def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > radiusSquared) 0.0
    else c * Math.pow(Math.E, -(magnitudeSquared / denominator))
  }
}


case class EpanechnikovKernel[V <: VectorData with Vector[V]](radius: Double) extends Kernel[V] {
  def weight(v: V): Double = weight(v.normSquared)

  def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > radiusSquared) 0.0
    else 0.75 - 0.75 * (magnitudeSquared / radiusSquared)
  }

  //  def naiveWeight(v: VectorN): Double = naiveWeight(v.magnitudeSquared)
  //
  //  def naiveWeight(magnitudeSquared: Double): Double = {
  //    val l = Math.sqrt(magnitudeSquared) / radius
  //    if (l > 1) 0.0
  //    else 0.75 - 0.75 * (l * l)
  //  }
}


case class UniformKernel[V <: VectorData with Vector[V]](radius: Double) extends Kernel[V] {
  def weight(v: V): Double = weight(v.normSquared)

  def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > radiusSquared) 0.0
    else 1.0 / (π * radiusSquared)
  }
}


object DiscreteKernel {
  def apply[V <: VectorData with Vector[V]](k: Kernel[V]): DiscreteKernel[V] = {
    val maxMagSquared: Int = Math.ceil(k.radiusSquared).toInt
    val weights = new Array[Double](maxMagSquared + 1)
    for (d <- 0 to maxMagSquared) weights(d) = k.weight(d)
    DiscreteKernel[V](k.radius, weights)
  }
}


case class DiscreteKernel[V <: VectorData with Vector[V]](radius: Double, weights: Array[Double]) extends Kernel[V] {

  lazy val totalWeights:Double = {
    var total = 0.0
    val r = radius.toInt
    for (dy <- -r to r; dx <- -r to r) total = total + weight(dx*dx + dy*dy)
    total
  }

  override def weight(v: V): Double = weight(v.normSquared)

  def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > radiusSquared) 0.0
    else weights(magnitudeSquared.toInt)
  }

}
