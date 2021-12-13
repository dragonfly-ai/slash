package ai.dragonfly.math.vector

import ai.dragonfly.math.util.Demonstrable

/**
 * Created by clifton on 1/10/17.
 */

object Vector3 extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val i = Vector3(1, 0, 0)
    val j = Vector3(0, 1, 0)
    val k = Vector3(0, 0, 1)

    sb.append(s"i3 X j3 -> ${i X j}\n")
    sb.append(s"j3 X i3 -> ${j X i}\n")

    sb.append(s"i3 X k3 -> ${i X k}\n")
    sb.append(s"k3 X i3 -> ${k X i}\n")

    sb.append(s"j3 X k3 -> ${j X k}\n")
    sb.append(s"k3 X j3 -> ${k X j}\n")

    sb.append(s"i3 dot j3 -> ${i dot j}\n")
    sb.append(s"j3 dot i3 -> ${j dot i}\n")

    sb.append(s"i3 dot k3 -> ${i dot k}\n")
    sb.append(s"k3 dot i3 -> ${k dot i}\n")

    sb.append(s"j3 dot k3 -> ${j dot k}\n")
    sb.append(s"k3 dot j3 -> ${k dot j}\n")
  }

  override def name: String = "Vector3"
}

case class Vector3(var x: Double, var y: Double, var z: Double) extends Vector {

  override val dimension: Int = 3

  override def values: VectorValues = VectorValues(x, y, z)

  override def distanceSquaredTo(v0: Vector): Double = {
    if (v0.dimension == dimension) {
      val dx = x - v0.component(0)
      val dy = y - v0.component(1)
      val dz = z - v0.component(2)
      dx * dx + dy * dy + dz * dz
    } else throw MismatchedVectorDimensionsException(this, v0)
  }

  override def dot(v0: Vector): Double = {
    if (v0.dimension == dimension) x * v0.component(0) + y * v0.component(1) + z * v0.component(2)
    else throw MismatchedVectorDimensionsException(this, v0)
  }

  def X (v0: Vector): Vector3 = cross(v0)

  def cross(v0: Vector): Vector3 = {
    if (v0.dimension == dimension) new Vector3(
      y * v0.component(2) - z * v0.component(1), // u2*v3 - u3*v2,
      z * v0.component(0) - x * v0.component(2), // u3*v1 - u1*v3,
      x * v0.component(1) - y * v0.component(0)  // u1*v2 - u2*v1
    ) else throw MismatchedVectorDimensionsException(this, v0)
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
      case _ => throw ExtraDimensionalAccessException(this, i)
    }
  }

  override def magnitudeSquared(): Double = x*x + y*y + z*z


  override def add(v0: Vector): Vector3 = {
    if (v0.dimension == dimension) {
      x = x + v0.component(0)
      y = y + v0.component(1)
      z = z + v0.component(2)
      this
    } else throw MismatchedVectorDimensionsException(this, v0)
  }

  override def subtract(v0: Vector): Vector3 = {
    if (v0.dimension == dimension) {
      x = x - v0.component(0)
      y = y - v0.component(1)
      z = z - v0.component(2)
      this
    } else throw MismatchedVectorDimensionsException(this, v0)
  }

  override def copy(): Vector3 = Vector3(x, y, z)

  override def toString: String = s"[$x,$y,$z]"

  override def round(): Vector = {
    x = Math.round(x).toDouble
    y = Math.round(y).toDouble
    z = Math.round(z).toDouble
    this
  }
}