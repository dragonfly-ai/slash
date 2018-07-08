package ai.dragonfly.math.vector

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

/**
 * Created by clifton on 1/10/17.
 */
@JSExport
case class Vector3(var x: Double, var y: Double, var z: Double) extends Vector {

  override val dimension: Int = 3

  override def values: Array[Double] = Array[Double](x, y, z)

  override def jsValues: js.Array[Double] = js.Array[Double](x, y, z)

  override def distanceSquaredTo(v0: Vector): Double = {
    if (v0.dimension == dimension) {
      val dx = x - v0.component(0)
      val dy = y - v0.component(1)
      val dz = z - v0.component(2)
      dx * dx + dy * dy + dz * dz
    } else throw MismatchedVectorDimensionsException(
      s"distanceSquaredTo undefined on vectors with different dimensions:\n" +
      s"dim($this) = ${dimension}\n" +
      s"dim($v0) = ${v0.dimension}"
    )
  }

  override def divide(denominator: Double): Vector3 = scale(1.0/denominator)

  override def dot(v0: Vector): Double = {
    if (v0.dimension == dimension) x * v0.component(0) + y * v0.component(1) + z * v0.component(2)
    else throw MismatchedVectorDimensionsException(
      s"dot undefined on vectors with different dimensions:\n" +
      s"dim($this) = ${dimension}\n" +
      s"dim($v0) = ${v0.dimension}"
    )
  }

  @JSExport def X (v0: Vector): Vector3 = cross(v0)

  @JSExport def cross(v0: Vector): Vector3 = {
    if (v0.dimension == dimension) new Vector3(
      y * v0.component(2) - z * v0.component(1), // u2*v3 - u3*v2,
      z * v0.component(0) - x * v0.component(2), // u3*v1 - u1*v3,
      x * v0.component(1) - y * v0.component(0)  // u1*v2 - u2*v1
    )
    else throw MismatchedVectorDimensionsException(
      s"pseudoCross undefined on vectors with different dimensions:\n" +
        s"dim($this) = ${dimension}\n" +
        s"dim($v0) = ${v0.dimension}"
    )
  }
  override def scale(scalar: Double): Vector3 = {
    x = x * scalar
    y = y * scalar
    z = z * scalar
    this
  }

  override def component(i: Int): Double = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new ArrayIndexOutOfBoundsException(s"index: $i exceeds the range [0, 1] for Vector2 components.")
    }
  }

  override def magnitudeSquared(): Double = x*x + y*y + z*z


  override def normalize(): Vector3 = {
    val mag2 = x*x + y*y + z*z
    if (mag2 > 0.0) {
      val mag = Math.sqrt(mag2)
      x = x / mag
      y = y / mag
      z = z / mag
    }
    this
  }

  override def add(v0: Vector): Vector3 = {
    if (v0.dimension == dimension) {
      x = x + v0.component(0)
      y = y + v0.component(1)
      z = z + v0.component(2)
      this
    } else throw MismatchedVectorDimensionsException(
      s"add undefined on vectors with different dimensions:\n" +
        s"dim($this) = ${dimension}\n" +
        s"dim($v0) = ${v0.dimension}"
    )
  }

  override def subtract(v0: Vector): Vector3 = {
    if (v0.dimension == dimension) {
      x = x - v0.component(0)
      y = y - v0.component(1)
      z = z - v0.component(2)
      this
    } else throw MismatchedVectorDimensionsException(
      s"add undefined on vectors with different dimensions:\n" +
        s"dim($this) = ${dimension}\n" +
        s"dim($v0) = ${v0.dimension}"
    )
  }

  override def copy(): Vector3 = Vector3(x, y, z)

  override def toString(): String = s"[$x,$y,$z]"

  override def round(): Vector = {
    x = Math.round(x)
    y = Math.round(y)
    z = Math.round(z)
    this
  }
}