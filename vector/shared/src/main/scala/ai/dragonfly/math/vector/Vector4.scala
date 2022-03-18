package ai.dragonfly.math.vector

import ai.dragonfly.math.example.Demonstrable
import scala.language.postfixOps
import ai.dragonfly.math.squareInPlace

object Vector4 extends VectorCompanion[Vector4] with Demonstrable {

  given dimension: Int = 4

  def apply(values:VectorValues): Vector4 = {
    if (values.length == 4) Vector4(values(0), values(1), values(2), values(3))
    else throw UnsupportedVectorDimension(values.length)
  }

  def fill(value:Double): Vector4 = Vector4(value, value, value, value)
  def fill(f: Int => Double):Vector4 = Vector4(f(0), f(1), f(2), f(3))

  def random(maxNorm:Double = 1.0): Vector4 = Vector4(maxNorm * Math.random(), maxNorm * Math.random(), maxNorm * Math.random(), maxNorm * Math.random())


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

    val v0:Vector4 = Vector4(0.5, 0.0, 1.0, 0.75)
    sb.append("val v₀ = Vector4(0.5, 0.0, 1.0, 0.75)")
      .append("\n\tv₀:").append(v0)
    sb.append("\nv₀.scale(3):\n\t").append(v0.scale(3.0)).append(" /* in place operation */")
    val v1:Vector4 = Vector4(5, 6, 7, 8)
    sb.append("\nv₁ = ").append(v1)
    sb.append("\nv₁.add(v1) = ").append(v1.add(v1)).append(" /* in place operation */")
    val v2:Vector4 = Vector4(0.25, 0.25, 0.25, 0.25)
    sb.append("\nv₂ = ").append(v2)
    sb.append("\nv₂.dot(v₀) = ").append(v2.dot(v0))
    sb.append("\nv₂ = ").append(v2)
    sb.append("\nv₂.subtract(v₀) = ").append(v2.subtract(v0)).append(" /* in place operation */")

    for (i <- 0 until 10) {
      val vT:Vector4 = Vector4.random(10.0)
      sb.append("\nval vT = Vector4.random() = ").append(vT)
        .append("\n\t∥").append(vT).append("∥ = ").append(vT.magnitude())
        .append("\n\tvT.normalize = ").append(vT.normalize()).append(" /* in place operation */")
        .append("\n\t∥").append(vT).append("∥ = ").append(vT.magnitude())
        .append("\n\t").append(vT).append(" * 2.0 = ").append(vT * 2).append(" /* Copy operation */")
        .append("\n\tvT remains unnaffected: ").append(vT)
    }
    sb.append("\n")
  }

  override def name: String = "Vector4"
}

case class Vector4(var x: Double, var y: Double, var z: Double, var w: Double) extends Vector {
  override def values: VectorValues = VectorValues(x, y, z, w)

  override inline def dimension: Int = 4

  override def component(i: Int): Double = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => z
      case _ => throw ExtraDimensionalAccessException(this, i)
    }
  }

  override inline def magnitudeSquared(): Double = squareInPlace(x) + squareInPlace(y) + squareInPlace(z) + squareInPlace(w)

  inline def distanceSquaredTo(v: Vector4): Double = {
    squareInPlace(x - v.x) + squareInPlace(y - v.y) + squareInPlace(z - v.z) + squareInPlace(w - v.w)
  }

  inline def distanceTo(v: Vector4): Double = Math.sqrt(distanceSquaredTo(v))

  def normalize():Vector4 = {
    val m2:Double = magnitudeSquared()
    if (m2 > 0.0) divide(Math.sqrt(m2))
    else throw VectorNormalizationException(this)
  }

  inline def dot(v0: Vector4): Double = {
    x * v0.x + y * v0.y + z * v0.z + w * v0.w
  }

  inline def += : (Vector4 => Vector4) = add
  inline def -= : (Vector4 => Vector4) = subtract
  inline def *= (scalar: Double): Vector4 =  scale(scalar)
  inline def /= (divisor: Double): Vector4 = divide(divisor)

  inline def scale(scalar: Double): Vector4 = {
    x = x * scalar
    y = y * scalar
    z = z * scalar
    w = w * scalar
    this
  }

  inline def divide(divisor: Double): Vector4 = scale(1 / divisor)

  def round(): Vector4 = {
    x = Math.round(x).toDouble
    y = Math.round(y).toDouble
    z = Math.round(z).toDouble
    w = Math.round(w).toDouble
    this
  }

  def add(v0: Vector4): Vector4 = {
      x = x + v0.x
      y = y + v0.y
      z = z + v0.z
      w = w + v0.w
      this
  }

  def subtract(v0: Vector4): Vector4 = {
      x = x - v0.x
      y = y - v0.y
      z = z - v0.z
      w = w - v0.w
      this
  }

  override def copy(): Vector4 = Vector4(x, y, z, w)

  // copy operators
  inline def *(scalar:Double):Vector4 = copy().*=(scalar)
  inline def /(divisor:Double):Vector4 = copy()./=(divisor)

  inline def +(v0:Vector4):Vector4 = copy().+=(v0)
  inline def -(v0:Vector4):Vector4 = copy().-=(v0)

  override def toString: String = s"《⁴↗〉${x}ᵢ ${y}ⱼ ${z}ₖ ${w}ₗ〉"

}
