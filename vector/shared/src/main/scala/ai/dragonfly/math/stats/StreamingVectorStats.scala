package ai.dragonfly.math.stats

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import ai.dragonfly.math.vector._

/**
 * Created by clifton on 1/10/17.
 */

@JSExportTopLevel("StreamingVectorStats")
class StreamingVectorStats(val dimension: Int) {
  var s0: Double = 0.0
  val s1: Array[Double] = Array.fill[Double](dimension)(0.0)
  val s2: Array[Double] = Array.fill[Double](dimension)(0.0)

  @JSExport def reset():Unit = {
    s0 = 0.0
    for (i <- 0 until dimension) {
      s1(i) = 0.0
      s2(i) = 0.0
    }
  }

  @JSExport("apply") def apply(c: Vector, weight: Double = 1.0): StreamingVectorStats = {
    s0 = s0 + weight
    for (i <- 0 until dimension) {
      val wci = c.component(i) * weight
      s1(i) = s1(i) + wci
      s2(i) = s2(i) + wci * wci
    }
    this
  }

  @JSExport def average(): Vector = {
    dimension match {
      case 2 => new Vector2(s1(0)/s0, s1(1)/s0)
      case 3 => new Vector3(s1(0)/s0, s1(1)/s0, s1(2)/s0)
      case _ => {
        val values:Array[Double] = Array.fill[Double](dimension)(0.0)
        for (i <- 0 until dimension) {
          values(i) = s1(i)/s0
        }
        new VectorN(values)
      }
    }
  }

  private def variance(s1d: Double, s2d: Double): Double = (s0 * s2d - s1d * s1d)/(s0 * (s0 - 1))

  @JSExport def variance(): Vector = {
    dimension match {
      case 2 => new Vector2(variance(s1(0), s2(0)), variance(s1(1), s2(1)))
      case 3 => new Vector3(variance(s1(0), s2(0)), variance(s1(1), s2(1)), variance(s1(2), s2(2)))
      case _ => {
        val values:Array[Double] = Array.fill[Double](dimension)(0.0)
        for (i <- 0 until dimension) {
          values(i) = variance(s1(i), s2(i))
        }
        new VectorN(values)
      }
    }
  }

  @JSExport def standardDeviation(): Vector = {
    variance() match {
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
        val values:Array[Double] = v.values
        for (i <- 0 until dimension) {
          values(i) = variance(s1(i), s2(i))
        }
        new VectorN(values)
    }
  }

  override def toString(): String = s"StreamingVectorStats(\n\t$s0\n\t${new VectorN(s1)}\n\t${new VectorN(s2)}\n)"
}
