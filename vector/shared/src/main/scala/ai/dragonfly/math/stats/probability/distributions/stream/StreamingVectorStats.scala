package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.Random.*
import ai.dragonfly.math.example.Demonstrable
import ai.dragonfly.math.vector._
import Vector.*

/**
 * Created by clifton on 1/10/17.
 */


object StreamingVectorStats extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val svs = new StreamingVectorStats(4)
    for (i <- 0 until 10000) svs(defaultRandom.nextVector4(1000))
    sb.append(svs).append("\n")
  }

  override def name: String = "StreamingVectorStats"
}

/**
 * Not thread safe!
 * @param dimension
 */
class StreamingVectorStats(val dimension: Int) {
  var s0: Double = 0.0
  val s1: VectorValues = VectorValues.fill(dimension)(0.0)
  val s2: VectorValues = VectorValues.fill(dimension)(0.0)

  val minValues: VectorValues = VectorValues.fill(dimension)(Double.MaxValue)
  val maxValues: VectorValues = VectorValues.fill(dimension)(Double.MinValue)

  def reset():Unit = synchronized {
    s0 = 0.0
    for (i <- 0 until dimension) {
      s1(i) = 0.0
      s2(i) = 0.0
      minValues(i) = Double.MaxValue
      maxValues(i) = Double.MinValue
    }
  }

  def apply(c: Vector, weight: Double = 1.0): StreamingVectorStats = synchronized {
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

  def average(): Vector = {
    dimension match {
      case 2 => Vector2(s1(0)/s0, s1(1)/s0)
      case 3 => Vector3(s1(0)/s0, s1(1)/s0, s1(2)/s0)
      case _ => VectorN(VectorValues.tabulate(dimension)((i:Int) => s1(i)/s0))
    }
  }

  private def componentVariance(s1d: Double, s2d: Double): Double = (s0 * s2d - s1d * s1d)/(s0 * (s0 - 1))

  def variance: Vector = {
    dimension match {
      case 2 => Vector2(componentVariance(s1(0), s2(0)), componentVariance(s1(1), s2(1)))
      case 3 => Vector3(componentVariance(s1(0), s2(0)), componentVariance(s1(1), s2(1)), componentVariance(s1(2), s2(2)))
      case _ => VectorN(VectorValues.tabulate(dimension)((i:Int) => componentVariance(s1(i), s2(i))))
    }
  }

  def standardDeviation: Vector = {
    variance match {
      case v: Vector2 =>
        v.x = Math.sqrt(v.x)
        v.y = Math.sqrt(v.y)
        v
      case v: Vector3 =>
        v.x = Math.sqrt(v.x)
        v.y = Math.sqrt(v.y)
        v.y = Math.sqrt(v.y)
        v
      case v: Vector =>
        val values:native.VectorValues = v.values
        for (i <- 0 until dimension) {
          values(i) = Math.sqrt(componentVariance(s1(i), s2(i)))
        }
        new VectorN(values)
    }
  }

  def bounds(): VectorBounds = VectorBounds(new VectorN(minValues), new VectorN(maxValues))

  override def toString: String = s"StreamingVectorStats(\n\t$s0\n\t${new VectorN(s1)}\n\t${new VectorN(s2)}\n\tAverage: ${average()}\n\tVariance: $variance\n\tStandard Deviation: $standardDeviation)"
}
