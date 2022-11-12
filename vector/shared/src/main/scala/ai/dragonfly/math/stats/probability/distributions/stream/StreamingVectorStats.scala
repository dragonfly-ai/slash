package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.Random.*
import ai.dragonfly.math.vector._
import narr.*

/**
 * Created by c on 1/10/17.
 */


/**
 * Not thread safe!
 * @param dimension
 */
class StreamingVectorStats[V <: ai.dragonfly.math.vector.Vector](val dimension: Int) {
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

  def apply(c: Vector, weight: Double = 1.0): StreamingVectorStats[V] = synchronized {
    s0 = s0 + weight
    for (i <- 0 until dimension) {
      val cv = c.component(i)
      val wci = cv * weight
      s1(i) = s1(i) + wci
      s2(i) = s2(i) + wci * wci
      minValues(i) = Math.min(minValues(i), cv)
      maxValues(i) = Math.max(maxValues(i), cv)
    }
    this
  }

  def average(): V = Vector.tabulate(dimension)(i => s1(i)/s0).asInstanceOf[V]

  private def componentVariance(s1d: Double, s2d: Double): Double = (s0 * s2d - s1d * s1d)/(s0 * (s0 - 1))

  def variance: V = Vector.tabulate(dimension)(i => componentVariance(s1(i), s2(i))).asInstanceOf[V]

  def standardDeviation: V = Vector.tabulate(dimension)(i => Math.sqrt(componentVariance(s1(i), s2(i)))).asInstanceOf[V]

  def bounds(): VectorBounds[V] = VectorBounds[V](
    Vector(minValues).asInstanceOf[V],
    Vector(maxValues).asInstanceOf[V]
  )

  override def toString: String = s"StreamingVectorStats(\n\t$s0\n\t${Vector(s1)}\n\t${Vector(s2)}\n\tAverage: ${average()}\n\tVariance: $variance\n\tStandard Deviation: $standardDeviation)"
}
