package ai.dragonfly.math.vector

import ai.dragonfly.math.examples.Demonstrable

/**
 * Created by clifton on 1/10/17.
 */

object Vector3 extends Demonstrable {

  def apply(values:VectorValues): Vector3 = {
    if (values.length == 3) Vector3(values(0), values(1), values(2))
    else throw UnsupportedVectorDimension(values.length)
  }

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

  override def distanceSquaredTo(v⃑: Vector): Double = {
    if (v⃑.dimension == dimension) {
      val dx = x - v⃑.component(0)
      val dy = y - v⃑.component(1)
      val dz = z - v⃑.component(2)
      dx * dx + dy * dy + dz * dz
    } else throw MismatchedVectorDimensionsException(this, v⃑)
  }

  override def dot(v⃑: Vector): Double = {
    if (v⃑.dimension == dimension) x * v⃑.component(0) + y * v⃑.component(1) + z * v⃑.component(2)
    else throw MismatchedVectorDimensionsException(this, v⃑)
  }

  def ⨯ (v⃑: Vector): Vector3 = cross(v⃑)

  def X (v⃑: Vector): Vector3 = cross(v⃑)

  def cross(v⃑: Vector): Vector3 = {
    if (v⃑.dimension == dimension) new Vector3(
      y * v⃑.component(2) - z * v⃑.component(1), // u2*v3 - u3*v2,
      z * v⃑.component(0) - x * v⃑.component(2), // u3*v1 - u1*v3,
      x * v⃑.component(1) - y * v⃑.component(0)  // u1*v2 - u2*v1
    ) else throw MismatchedVectorDimensionsException(this, v⃑)
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


  override def add(v⃑: Vector): Vector3 = {
    if (v⃑.dimension == dimension) {
      x = x + v⃑.component(0)
      y = y + v⃑.component(1)
      z = z + v⃑.component(2)
      this
    } else throw MismatchedVectorDimensionsException(this, v⃑)
  }

  override def subtract(v⃑: Vector): Vector3 = {
    if (v⃑.dimension == dimension) {
      x = x - v⃑.component(0)
      y = y - v⃑.component(1)
      z = z - v⃑.component(2)
      this
    } else throw MismatchedVectorDimensionsException(this, v⃑)
  }

  override def copy(): Vector3 = Vector3(x, y, z)

  override def toString: String = s"《³↗〉${x}ᵢ ${y}ⱼ ${z}ₖ〉"

  override def round(): Vector = {
    x = Math.round(x).toDouble
    y = Math.round(y).toDouble
    z = Math.round(z).toDouble
    this
  }
}