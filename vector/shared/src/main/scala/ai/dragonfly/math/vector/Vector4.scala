package ai.dragonfly.math.vector

import ai.dragonfly.math.examples.Demonstrable

object Vector4 extends Demonstrable {

  def apply(values:VectorValues): Vector4 = {
    if (values.length == 4) Vector4(values(0), values(1), values(2), values(3))
    else throw UnsupportedVectorDimension(values.length)
  }

  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val i = Vector4(1, 0, 0, 0)
    val j = Vector4(0, 1, 0, 0)
    val k = Vector4(0, 0, 1, 0)
    val l = Vector4(0, 0, 0, 1)

    sb.append(s"i4 dot j4 -> ${i dot j}\n")
    sb.append(s"j4 dot i4 -> ${j dot i}\n")

    sb.append(s"i4 dot k4 -> ${i dot k}\n")
    sb.append(s"k4 dot i4 -> ${k dot i}\n")

    sb.append(s"j4 dot k4 -> ${j dot k}\n")
    sb.append(s"k4 dot j4 -> ${k dot j}\n")

    sb.append(s"i4 dot l4 -> ${i dot l}\n")
    sb.append(s"l4 dot i4 -> ${l dot i}\n")

    sb.append(s"j4 dot l4 -> ${j dot l}\n")
    sb.append(s"l4 dot j4 -> ${l dot j}\n")

    sb.append(s"k4 dot l4 -> ${k dot l}\n")
    sb.append(s"l4 dot k4 -> ${l dot k}\n")
  }

  override def name: String = "Vector4"
}

case class Vector4(var x: Double, var y: Double, var z: Double, var w: Double) extends Vector {
  override def values: VectorValues = VectorValues(x, y, z, w)

  override def dimension: Int = 4

  override def component(i: Int): Double = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => z
      case _ => throw ExtraDimensionalAccessException(this, i)
    }
  }

  override def magnitudeSquared(): Double = x*x + y*y + z*z + w*w

  override def distanceSquaredTo(v⃑: Vector): Double = {
    if (v⃑.dimension == dimension) {
      val dx = x - v⃑.component(0)
      val dy = y - v⃑.component(1)
      val dz = z - v⃑.component(2)
      val dw = w - v⃑.component(3)
      dx * dx + dy * dy + dz * dz + dw * dw
    } else throw MismatchedVectorDimensionsException(this, v⃑)
  }

  override def dot(v⃑: Vector): Double = {
    if (v⃑.dimension == dimension) x * v⃑.component(0) + y * v⃑.component(1) + z * v⃑.component(2) + w * v⃑.component(3)
    else throw MismatchedVectorDimensionsException(this, v⃑)
  }

  override def scale(scalar: Double): Vector4 = {
    x = x * scalar
    y = y * scalar
    z = z * scalar
    w = w * scalar
    this
  }

  override def round(): Vector4 = {
    x = Math.round(x).toDouble
    y = Math.round(y).toDouble
    z = Math.round(z).toDouble
    w = Math.round(w).toDouble
    this
  }

  override def add(v⃑: Vector): Vector4 = {
    if (v⃑.dimension == dimension) {
      x = x + v⃑.component(0)
      y = y + v⃑.component(1)
      z = z + v⃑.component(2)
      w = w + v⃑.component(3)
      this
    } else throw MismatchedVectorDimensionsException(this, v⃑)
  }

  override def subtract(v⃑: Vector): Vector4 = {
    if (v⃑.dimension == dimension) {
      x = x - v⃑.component(0)
      y = y - v⃑.component(1)
      z = z - v⃑.component(2)
      w = w - v⃑.component(3)
      this
    } else throw MismatchedVectorDimensionsException(this, v⃑)
  }

  override def copy(): Vector4 = Vector4(x, y, z, w)

  override def toString: String = s"《⁴↗〉${x}ᵢ ${y}ⱼ ${z}ₖ ${w}ₗ〉"

}
