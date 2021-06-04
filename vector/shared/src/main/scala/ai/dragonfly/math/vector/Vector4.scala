package ai.dragonfly.math.vector

import ai.dragonfly.math.util.Demonstrable

import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
object Vector4 extends Demonstrable {
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

  override def distanceSquaredTo(v0: Vector): Double = {
    if (v0.dimension == dimension) {
      val dx = x - v0.component(0)
      val dy = y - v0.component(1)
      val dz = z - v0.component(2)
      val dw = w - v0.component(3)
      dx * dx + dy * dy + dz * dz + dw * dw
    } else throw MismatchedVectorDimensionsException(this, v0)
  }

  override def dot(v0: V): Double = {
    if (v0.dimension == dimension) x * v0.component(0) + y * v0.component(1) + z * v0.component(2) + w * v0.component(3)
    else throw MismatchedVectorDimensionsException(this, v0)
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

  override def add(v0: Vector): Vector4 = {
    if (v0.dimension == dimension) {
      x = x + v0.component(0)
      y = y + v0.component(1)
      z = z + v0.component(2)
      w = w + v0.component(3)
      this
    } else throw MismatchedVectorDimensionsException(this, v0)
  }

  override def subtract(v0: Vector): Vector4 = {
    if (v0.dimension == dimension) {
      x = x - v0.component(0)
      y = y - v0.component(1)
      z = z - v0.component(2)
      w = w - v0.component(3)
      this
    } else throw MismatchedVectorDimensionsException(this, v0)
  }

  override def copy(): Vector4 = Vector4(x, y, z, w)
}
