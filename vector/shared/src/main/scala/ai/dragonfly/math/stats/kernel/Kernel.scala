/*
 * Copyright 2023 dragonfly.ai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

trait Kernel[V <: ai.dragonfly.math.vector.Vector] {
  val exclusionRadius: Double
  lazy val exclusionRadiusSquared:Double = exclusionRadius * exclusionRadius

  def weight(magnitudeSquared: Double): Double
  def weight(v: V): Double
  def weight(v1: V, v2: V): Double = weight((v1 - v2).asInstanceOf[V])
  def distance(v: V): Double = v.norm
  def distance(v1: V, v2: V): Double = (v1 - v2).norm

  lazy val discretize: DiscreteKernel[V] = DiscreteKernel[V](this)
}


case class GaussianKernel[V <: ai.dragonfly.math.vector.Vector](exclusionRadius: Double, gaussian:Gaussian) extends Kernel[V] {

  override def weight(v: V): Double = weight(v.normSquared)

  override def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else gaussian.p2(magnitudeSquared)
  }
}


case class EpanechnikovKernel[V <: ai.dragonfly.math.vector.Vector](exclusionRadius: Double) extends Kernel[V] {
  override def weight(v: V): Double = weight(v.normSquared)

  override def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else 0.75 - 0.75 * (magnitudeSquared / exclusionRadiusSquared)
  }

}


case class UniformKernel[V <: ai.dragonfly.math.vector.Vector](exclusionRadius: Double) extends Kernel[V] {
  override def weight(v: V): Double = weight(v.normSquared)

  override def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else 1.0 / (π * exclusionRadiusSquared)
  }
}


object DiscreteKernel {
  def apply[V <: ai.dragonfly.math.vector.Vector](k: Kernel[V]): DiscreteKernel[V] = {
    val maxMagSquared: Int = Math.ceil(k.exclusionRadiusSquared).toInt
    val weights = new Array[Double](maxMagSquared + 1)
    for (d <- 0 to maxMagSquared) weights(d) = k.weight(d)
    DiscreteKernel(k.exclusionRadius, weights)
  }
}


case class DiscreteKernel[V <: ai.dragonfly.math.vector.Vector](exclusionRadius: Double, weights: Array[Double]) extends Kernel[V] {

  lazy val totalWeights:Double = {
    var total = 0.0
    val r = exclusionRadius.toInt
    for (dy <- -r to r; dx <- -r to r) total += weight(squareInPlace(dx) + squareInPlace(dy))
    total
  }

  override def weight(v: V): Double = weight(v.normSquared)

  override def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else weights(magnitudeSquared.toInt)
  }

}
