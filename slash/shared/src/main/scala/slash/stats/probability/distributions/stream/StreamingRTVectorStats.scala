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

import slash.vector.runtime.*

/**
 * Created by c on 1/10/17.
 */

class StreamingRTVectorStats(dimension:Int) {  // Why doesn't this extend Sampleable?
  private val internal = new util.StreamingNArrayStats(dimension)

  def reset():Unit = internal.reset()

  def apply(c: RTVec, weight: Double = 1.0): StreamingRTVectorStats = {
    internal.apply(c.asNativeArray, weight)
    this
  }

  inline def minValues(i:Int):Double = internal.minValues(i)
  inline def maxValues(i:Int):Double = internal.maxValues(i)

  inline def average(): RTVec = internal.average().asInstanceOf[RTVec]

  inline def variance: RTVec = internal.variance.asInstanceOf[RTVec]

  inline def standardDeviation: RTVec = internal.standardDeviation.asInstanceOf[RTVec]

  inline def bounds(): RTVectorBounds = RTVectorBounds(
    RTVec(internal.minValues),
    RTVec(internal.maxValues)
  )

  override def toString: String = s"StreamingRTVectorStats(" +
    s"\n\ts0: ${internal.s0}\n\ts1: ${RTVec(internal.s1).render()}" +
    s"\n\ts2: ${RTVec(internal.s2).render()}" +
    s"\n\tminValues: ${RTVec(internal.minValues).render()}" +
    s"\n\tmaxValues: ${RTVec(internal.maxValues).render()}" +
    s"\n\tAverage: ${average().render()}" +
    s"\n\tVariance: ${variance.render()}" +
    s"\n\tStandard Deviation: ${standardDeviation.render()})"

}
