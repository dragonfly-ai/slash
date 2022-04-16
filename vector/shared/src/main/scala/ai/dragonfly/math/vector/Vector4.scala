package ai.dragonfly.math.vector

import ai.dragonfly.math.example.Demonstrable
import scala.language.postfixOps
import ai.dragonfly.math.squareInPlace

object Vector4 extends VectorCompanion[Vector4] with Demonstrable {

  given dimension: Int = 4

  inline override def validDimension(dimension: Int): Boolean = dimension == 4

  def apply(values:VectorValues): Vector4 = {
    if (values.length == 4) new Vector4(values)
    else throw UnsupportedVectorDimension(values.length)
  }

  def apply(x: Double, y: Double, z: Double, w: Double):Vector4 = new Vector4(VectorValues(x, y, z, w))


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
        .append("\n\t∥").append(vT).append("∥ = ").append(vT.norm)
        .append("\n\tvT.normalize = ").append(vT.normalize()).append(" /* in place operation */")
        .append("\n\t∥").append(vT).append("∥ = ").append(vT.norm)
        .append("\n\t").append(vT).append(" * 2.0 = ").append(vT * 2).append(" /* Copy operation */")
        .append("\n\tvT remains unnaffected: ").append(vT)
    }
    sb.append("\n")
  }

  override def name: String = "Vector4"
}

case class Vector4 private (override val values: VectorValues) extends Vector {

  type VEC = Vector4

  inline def x:Double = values(0)
  inline def y:Double = values(1)
  inline def z:Double = values(2)
  inline def w:Double = values(3)

  override inline def copy(): VEC = Vector4(x, y, z, w)

  override def toString: String = s"《⁴↗〉${x}ᵢ ${y}ⱼ ${z}ₖ ${w}ₗ〉"

}
