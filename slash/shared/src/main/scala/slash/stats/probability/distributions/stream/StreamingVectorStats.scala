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

package slash.stats.probability.distributions.stream

import slash.vector.*
import narr.*
import slash.vector.VectorBounds

/**
 * Created by c on 1/10/17.
 */



class StreamingVectorStats[N <: Int](using ValueOf[N]) {  // Why doesn't this extend Sampleable?
  val dimension: N = valueOf[N]

  var s0: Double = 0.0
  val s1: NArray[Double] = NArray.fill[Double](dimension)(0.0)
  val s2: NArray[Double] = NArray.fill[Double](dimension)(0.0)

  val minValues: NArray[Double] = NArray.fill[Double](dimension)(Double.MaxValue)
  val maxValues: NArray[Double] = NArray.fill[Double](dimension)(Double.MinValue)

  def reset():Unit = synchronized {
    s0 = 0.0
    for (i <- 0 until dimension) {
      s1(i) = 0.0
      s2(i) = 0.0
      minValues(i) = Double.MaxValue
      maxValues(i) = Double.MinValue
    }
  }

  def apply(c: Vec[N], weight: Double = 1.0): StreamingVectorStats[N] = synchronized {
    s0 = s0 + weight
    for (i <- 0 until dimension) {
      val cv = c(i)
      val wci = cv * weight
      s1(i) = s1(i) + wci
      s2(i) = s2(i) + wci * wci
      minValues(i) = Math.min(minValues(i), cv)
      maxValues(i) = Math.max(maxValues(i), cv)
    }
    this
  }

  inline def average(): Vec[N] = Vec.tabulate[N](i => s1(i)/s0)

  private def componentVariance(s1d: Double, s2d: Double): Double = (s0 * s2d - s1d * s1d)/(s0 * (s0 - 1))

  inline def variance: Vec[N] = Vec.tabulate[N](i => componentVariance(s1(i), s2(i)))

  inline def standardDeviation: Vec[N] = Vec.tabulate[N](i => Math.sqrt(componentVariance(s1(i), s2(i))))

  inline def bounds(): VectorBounds[N] = VectorBounds[N](
    Vec[N](minValues),
    Vec[N](maxValues)
  )

  override def toString: String = s"StreamingVectorStats(\n\t$s0\n\t${Vec[N](s1).render()}\n\t${Vec[N](s2).render()}\n\tAverage: ${average().render()}\n\tVariance: ${variance.render()}\n\tStandard Deviation: ${standardDeviation.render()})"
}
