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

import ai.dragonfly.math.*
import Constant.{`√(2π)`, π}
import ai.dragonfly.math.stats.DenseHistogramOfContinuousDistribution
import ai.dragonfly.math.stats.probability.distributions.Gaussian
import ai.dragonfly.math.vector.*
import Vec.*
import narr.*
import scala.collection.mutable

trait Kernel[N <: Int] {
  val exclusionRadius: Double
  lazy val exclusionRadiusSquared:Double = exclusionRadius * exclusionRadius

  def weight(magnitudeSquared: Double): Double
  def weight(v: Vec[N]): Double
  def weight(v1: Vec[N], v2: Vec[N]): Double = weight(v1 - v2)
  def distance(v: Vec[N]): Double = v.norm
  def distance(v1: Vec[N], v2: Vec[N]): Double = (v1 - v2).norm

  lazy val discretize: DiscreteKernel[N] = DiscreteKernel[N](this)
}


case class GaussianKernel[N <: Int](exclusionRadius: Double, gaussian:Gaussian) extends Kernel[N] {

  override def weight(v: Vec[N]): Double = weight(v.normSquared)

  override def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else gaussian.p2(magnitudeSquared)
  }
}


case class EpanechnikovKernel[N <: Int](exclusionRadius: Double) extends Kernel[N] {
  override def weight(v: Vec[N]): Double = weight(v.normSquared)

  override def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else 0.75 - 0.75 * (magnitudeSquared / exclusionRadiusSquared)
  }

}


case class UniformKernel[N <: Int](exclusionRadius: Double) extends Kernel[N] {
  override def weight(v: Vec[N]): Double = weight(v.normSquared)

  override def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else 1.0 / (π * exclusionRadiusSquared)
  }
}


object DiscreteKernel {
  def apply[N <: Int](k: Kernel[N]): DiscreteKernel[N] = {
    val maxMagSquared: Int = Math.ceil(k.exclusionRadiusSquared).toInt
    val weights = new NArray[Double](maxMagSquared + 1)
    for (d <- 0 to maxMagSquared) weights(d) = k.weight(d)
    DiscreteKernel(k.exclusionRadius, weights)
  }
}


case class DiscreteKernel[N <: Int](exclusionRadius: Double, weights: NArray[Double]) extends Kernel[N] {

  lazy val totalWeights:Double = {
    var total = 0.0
    val r = exclusionRadius.toInt
    for (dy <- -r to r; dx <- -r to r) total += weight(squareInPlace(dx) + squareInPlace(dy))
    total
  }

  override def weight(v: Vec[N]): Double = weight(v.normSquared)

  override def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else weights(magnitudeSquared.toInt)
  }

}
