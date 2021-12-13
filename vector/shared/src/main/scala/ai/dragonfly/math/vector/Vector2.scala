package ai.dragonfly.math.vector

import ai.dragonfly.math.util.Demonstrable

import scala.scalajs.js

/**
 * Created by clifton on 1/10/17.
 */

object Vector2 extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    for (deg <- Array[Double](10, 25, 33.333333, 45, 60, 75, 90)) {
      val i = Vector2(1, 0)
      i.rotateDegrees(deg)
      sb.append(s"${Vector2(1, 0)}.rotateDegrees($deg) -> $i\n")
    }
    sb
  }

  override def name: String = "Vector2"
}

case class Vector2(var x: Double, var y: Double) extends Vector {

  override val dimension: Int = 2

  override def values: VectorValues = VectorValues(x, y)

  override def distanceSquaredTo(v0: Vector): Double = {
    if (v0.dimension == dimension) {
      val dx = x - v0.component(0)
      val dy = y - v0.component(1)
      dx * dx + dy * dy
    } else throw MismatchedVectorDimensionsException(this, v0)
  }

  override def dot(v0: Vector): Double = {
    if (v0.dimension == dimension) x * v0.component(0) + y * v0.component(1)
    else throw MismatchedVectorDimensionsException(this, v0)
  }

  def pseudoCross(v0: Vector): Double = {
    if (v0.dimension == dimension) x * v0.component(1) + y * v0.component(0)
    else throw MismatchedVectorDimensionsException(this, v0)
  }

  override def scale(scalar: Double): Vector2 = {
    x = x * scalar
    y = y * scalar
    this
  }

  override def component(i: Int): Double = {
    if (i == 0) x else if (i == 1) y
    else throw ExtraDimensionalAccessException(this, i)
  }

  override def magnitudeSquared(): Double = x*x + y*y

  override def add(v0: Vector): Vector2 = {
    if (v0.dimension == dimension) {
      x = x + v0.component(0)
      y = y + v0.component(1)
      this
    } else throw MismatchedVectorDimensionsException(this, v0)
  }

  override def subtract(v0: Vector): Vector2 = {
    if (v0.dimension == dimension) {
      x = x - v0.component(0)
      y = y - v0.component(1)
      this
    } else throw MismatchedVectorDimensionsException(this, v0)
  }

  def rotateDegrees(degrees: Double): Vector2 = rotate(degrees * 0.01745329252)

  def rotate(radians: Double): Vector2 = {
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
