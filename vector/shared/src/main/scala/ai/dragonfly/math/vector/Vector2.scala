package ai.dragonfly.math.vector

import ai.dragonfly.math.example.Demonstrable
import ai.dragonfly.math.squareInPlace

import scala.scalajs.js

/**
 * Created by clifton on 1/10/17.
 */

object Vector2 extends VectorCompanion[Vector2] with Demonstrable {

  inline given dimension: Int = 2

  override inline def validDimension(dimension: Int): Boolean = dimension == this.dimension

  override def apply(values:VectorValues): Vector2 = {
    if (values.length == 2) Vector2(values(0), values(1))
    else throw UnsupportedVectorDimension(values.length)
  }

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

case class Vector2(var x: Double, var y: Double) extends VectorOps[Vector2] {

  override val dimension: Int = Vector2.dimension

  override def values: VectorValues = VectorValues(x, y)

  override def recognize(v: Vector): Vector2 = v.asInstanceOf[Vector2]

  override def component(i: Int): Double = {
    if (i == 0) x else if (i == 1) y
    else throw ExtraDimensionalAccessException(this, i)
  }

  override inline def euclideanNormSquared: Double = squareInPlace(x) + squareInPlace(y)

  override inline def euclideanDistanceSquaredTo(v: Vector2): Double = {
    squareInPlace(x - v.x) + squareInPlace(y - v.y)
  }


  override inline def dot(v: Vector2): Double = {
    x * v.x + y * v.y
  }

  inline def pseudoCross(v: Vector2): Double = {
    x * v.y + y * v.x
  }


  override inline def scale(scalar: Double): Vector2 = {
    x = x * scalar
    y = y * scalar
    this
  }

  override inline def divide(divisor: Double): Vector2 = {
    x = x / divisor
    y = y / divisor
    this
  }

  override inline def add(v: Vector2): Vector2 = {
      x = x + v.x
      y = y + v.y
      this
  }

  override inline def subtract(v: Vector2): Vector2 = {
      x = x - v.x
      y = y - v.y
      this
  }

  inline def rotateDegrees(degrees: Double): Vector2 = rotate(degrees * 0.01745329252)

  inline def rotate(radians: Double): Vector2 = {
    val cos = Math.cos( radians )
    val sin = Math.sin( radians )

    val x1 = x*cos - y*sin
    y = x*sin + y*cos
    x = x1

    this
  }

  override def copy(): Vector2 = Vector2(x, y)

  override def toString: String = s"《²↗〉${x}ᵢ ${y}ⱼ〉" // ₂⃗ ²↗ ↗²

  def round(): Vector2 = {
    x = Math.round(x).toDouble
    y = Math.round(y).toDouble
    this
  }
}
