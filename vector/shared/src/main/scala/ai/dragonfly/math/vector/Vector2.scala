package ai.dragonfly.math.vector

import ai.dragonfly.math.examples.Demonstrable
import ai.dragonfly.math.squareInPlace

import scala.scalajs.js

/**
 * Created by clifton on 1/10/17.
 */

object Vector2 extends VectorCompanion[Vector2] with Demonstrable {

  given dimension: Int = 2

  override def apply(values:VectorValues): Vector2 = {
    if (values.length == 2) Vector2(values(0), values(1))
    else throw UnsupportedVectorDimension(values.length)
  }

  def fill(value:Double): Vector2 = Vector2(value, value)

  def fill(f: Int => Double):Vector2 = Vector2(f(0), f(1))

  def random(maxNorm:Double = 1.0): Vector2 = Vector2(maxNorm * Math.random(), maxNorm * Math.random())


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

  override def component(i: Int): Double = {
    if (i == 0) x else if (i == 1) y
    else throw ExtraDimensionalAccessException(this, i)
  }

  override inline def magnitudeSquared(): Double = squareInPlace(x) + squareInPlace(y)

  inline def distanceSquaredTo(v: Vector): Double = {
    val v0:Vector2 = v.asInstanceOf[Vector2]
    squareInPlace(x - v0.x) + squareInPlace(y - v0.y)
  }

  def normalize():Vector2 = {
    val m2:Double = magnitudeSquared()
    if (m2 > 0.0) divide(Math.sqrt(m2))
    else throw VectorNormalizationException(this)
  }

  inline def dot(v: Vector2): Double = {
    x * v.x + y * v.y
  }

  def pseudoCross(v: Vector2): Double = {
    x * v.y + y * v.x
  }


  inline def +=(v: Vector2): Vector2 = add(v)
  inline def -=(v: Vector2): Vector2 = subtract(v)
  inline def *= (scalar: Double): Vector2 =  scale(scalar)
  inline def /= (divisor: Double): Vector2 = divide(divisor)


  inline def scale(scalar: Double): Vector2 = {
    x = x * scalar
    y = y * scalar
    this
  }

  inline def divide(divisor: Double): Vector2 = {
    x = x / divisor
    y = y / divisor
    this
  }

  inline def add(v: Vector2): Vector2 = {
      x = x + v.x
      y = y + v.y
      this
  }

  def subtract(v: Vector2): Vector2 = {
      x = x - v.x
      y = y - v.y
      this
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

  // copy operators
  def *(scalar:Double):Vector2 = copy().scale(scalar)
  def /(divisor:Double):Vector2 = copy().divide(divisor)

  def +(v:Vector2):Vector2 = copy().add(v)
  def -(v:Vector2):Vector2 = copy().subtract(v)

  override def toString: String = s"《²↗〉${x}ᵢ ${y}ⱼ〉" // ₂⃗ ²↗ ↗²

  def round(): Vector2 = {
    x = Math.round(x).toDouble
    y = Math.round(y).toDouble
    this
  }
}
