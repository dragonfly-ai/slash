package ai.dragonfly.math.vector

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/**
 * Created by clifton on 1/10/17.
 */

@JSExportTopLevel("Vector2")
case class Vector2(var x: Double, var y: Double) extends Vector {

  override val dimension: Int = 2

  override def values: Array[Double] = Array[Double](x, y)

  override def divide(denominator: Double): Vector2 = scale(1.0 / denominator)

  override def distanceSquaredTo(v0: Vector): Double = {
    if (v0.dimension == dimension) {
      val dx = x - v0.component(0)
      val dy = y - v0.component(1)
      dx * dx + dy * dy
    } else throw MismatchedVectorDimensionsException(
      s"distanceSquaredTo undefined on vectors with different dimensions:\n" +
      s"dim($this) = ${dimension}\n" +
      s"dim($v0) = ${v0.dimension}"
    )
  }

  override def dot(v0: Vector): Double = {
    if (v0.dimension == dimension) x * v0.component(0) + y * v0.component(1)
    else throw MismatchedVectorDimensionsException(
      s"dot undefined on vectors with different dimensions:\n" +
      s"dim($this) = ${dimension}\n" +
      s"dim($v0) = ${v0.dimension}"
    )
  }

  @JSExport def pseudoCross(v0: Vector): Double = {
    if (v0.dimension == dimension) x * v0.component(1) + y * v0.component(0)
    else throw MismatchedVectorDimensionsException(
      s"pseudoCross undefined on vectors with different dimensions:\n" +
      s"dim($this) = ${dimension}\n" +
      s"dim($v0) = ${v0.dimension}"
    )
  }

  override def scale(scalar: Double): Vector2 = {
    x = x * scalar
    y = y * scalar
    this
  }

  override def component(i: Int): Double = {
    if (i == 0) x else if (i == 1) y
    else throw new ArrayIndexOutOfBoundsException(s"index: $i exceeds the range [0, 1] for Vector2 components.")
  }

  override def magnitudeSquared(): Double = x*x + y*y


  override def normalize(): Vector2 = {
    val mag2 = x*x + y*y
    if (mag2 > 0.0) {
      val mag = Math.sqrt(mag2)
      x = x / mag
      y = y / mag
    }
    this
  }


  override def add(v0: Vector): Vector2 = {
    if (v0.dimension == dimension) {
      x = x + v0.component(0)
      y = y + v0.component(1)
      this
    } else throw MismatchedVectorDimensionsException(
      s"add undefined on vectors with different dimensions:\n" +
      s"dim($this) = ${dimension}\n" +
      s"dim($v0) = ${v0.dimension}"
    )
  }

  override def subtract(v0: Vector): Vector2 = {
    if (v0.dimension == dimension) {
      x = x - v0.component(0)
      y = y - v0.component(1)
      this
    } else throw MismatchedVectorDimensionsException(
      s"add undefined on vectors with different dimensions:\n" +
      s"dim($this) = ${dimension}\n" +
      s"dim($v0) = ${v0.dimension}"
    )
  }

  @JSExport def rotateDegrees(degrees: Double): Vector2 = rotate(degrees * 0.01745329252)

  @JSExport def rotate(radians: Double): Vector2 = {
    val cos = Math.cos( radians )
    val sin = Math.sin( radians )

    val x1 = x*cos - y*sin
    y = x*sin + y*cos
    x = x1

    this
  }

  override def copy(): Vector2 = Vector2(x, y)

  override def toString: String = s"[$x,$y]"

  override def round(): Vector = {
    x = Math.round(x).toDouble
    y = Math.round(y).toDouble
    this
  }
}
