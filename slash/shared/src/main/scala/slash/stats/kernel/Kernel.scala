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

package slash.stats.kernel

import slash.*
import Constant.*
import slash.stats.probability.distributions.Gaussian
import slash.vector.*
import slash.vector.runtime.RTVec
import narr.*

trait Kernel {

  val exclusionRadius: Double
  lazy val exclusionRadiusSquared:Double = exclusionRadius * exclusionRadius

  def weight(magnitudeSquared: Double): Double

  private def weigh(v:NArray[Double]): Double = weight(util.normSquared(v))
  private def weigh(v1:NArray[Double], v2:NArray[Double]): Double = weigh(util.vectorCopyAndSubtract(v1, v2))

  private def dist(v:NArray[Double]): Double = util.norm(v)
  private def dist(v1:NArray[Double], v2:NArray[Double]): Double = util.norm(util.vectorCopyAndSubtract(v1, v2))

  inline def weight[N <: Int](v:Vec[N]):Double = weigh(v.asNativeArray)
  inline def weight[N <: Int](v1:Vec[N], v2:Vec[N]):Double = weigh(v1.asNativeArray, v2.asNativeArray)

  inline def distance[N <: Int](v:Vec[N]):Double = dist(v.asNativeArray)
  inline def distance[N <: Int](v1:Vec[N], v2:Vec[N]): Double = dist(v1.asNativeArray, v2.asNativeArray)

  inline def rtWeight(v:RTVec):Double = weigh(v.asNativeArray)
  inline def rtWeight(v1:RTVec, v2:RTVec):Double = {
    dimensionCheck(v1.dimension, v2.dimension)
    weigh(v1.asNativeArray, v2.asNativeArray)
  }

  inline def rtDistance(v:RTVec):Double = dist(v.asNativeArray)
  inline def rtDistance(v1:RTVec, v2:RTVec): Double = {
    dimensionCheck(v1.dimension, v2.dimension)
    dist(v1.asNativeArray, v2.asNativeArray)
  }

  lazy val discretize: DiscreteKernel = DiscreteKernel(this)
}


case class GaussianKernel(exclusionRadius: Double, gaussian:Gaussian) extends Kernel {
  override def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else gaussian.p2(magnitudeSquared)
  }
}


case class EpanechnikovKernel(exclusionRadius: Double) extends Kernel {
  override def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else 0.75 - 0.75 * (magnitudeSquared / exclusionRadiusSquared)
  }
}


case class UniformKernel(exclusionRadius: Double) extends Kernel {
  override def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else 1.0 / (π * exclusionRadiusSquared)
  }
}


object DiscreteKernel {
  def apply(k: Kernel): DiscreteKernel = {
    val maxMagSquared: Int = Math.ceil(k.exclusionRadiusSquared).toInt
    val weights = new NArray[Double](maxMagSquared + 1)
    for (d <- 0 to maxMagSquared) weights(d) = k.weight(d)
    DiscreteKernel(k.exclusionRadius, weights)
  }
}


case class DiscreteKernel(exclusionRadius: Double, weights: NArray[Double]) extends Kernel {

  lazy val totalWeights:Double = {
    var total = 0.0
    val r = exclusionRadius.toInt
    //for (dy <- -r to r; dx <- -r to r)
    var dy:Int = -r
    while (dy <= r) {
      var dx: Int = -r
      while (dx <= r) {
        total += weight(squareInPlace(dx) + squareInPlace(dy))
        dx = dx + 1
      }
      dy = dy + 1
    }
    total
  }

  override def weight(magnitudeSquared: Double): Double = {
    if (magnitudeSquared > exclusionRadiusSquared) 0.0
    else weights(magnitudeSquared.toInt)
  }

}
