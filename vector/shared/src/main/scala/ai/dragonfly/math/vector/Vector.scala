package ai.dragonfly.math.vector

import scala.scalajs.js
import ai.dragonfly.math.vector.*

object Vector {

  def apply(values: VectorValues): Vector = {
    values.length match {
      case dim if dim < 2 => throw UnsupportedVectorDimension(dim)
      case 2 => Vector2(values(0), values(1))
      case 3 => Vector3(values(0), values(1), values(2))
      case 4 => Vector4(values(0), values(1), values(2), values(3))
      case _ => VectorN(values)
    }
  }

  def apply(d:Double*): Vector = {
    d.size match {
      case dim if dim < 2 => throw UnsupportedVectorDimension(dim)
      case 2 => Vector2(d(0), d(1))
      case 3 => Vector3(d(0), d(1), d(2))
      case 4 => Vector4(d(0), d(1), d(2), d(3))
      case _ => VectorN(VectorValues(d:_*))
    }
  }

  def midpoint[V <: Vector](v0: Vector, v1: Vector): V = ((v0 + v1) * 0.5).asInstanceOf[V]

  def average[V <: Vector](`[v₁v₂⋯vₙ]`:Vector*): V = ({
    val μ⃑:V = `[v₁v₂⋯vₙ]`.head.copy().asInstanceOf[V]
    for (vector <- `[v₁v₂⋯vₙ]`.tail) {
      μ⃑.add(vector)
    }
    μ⃑.scale(1.0/`[v₁v₂⋯vₙ]`.size)
  }).asInstanceOf[V]

  def average[V <: Vector](`[v₀v₁⋯v₍ₙ₋₁₎]`: VECTORS): V = ({
    val `¹/ₙ`:Double = 1.0 / `[v₀v₁⋯v₍ₙ₋₁₎]`.length
    val μ⃑:V = `[v₀v₁⋯v₍ₙ₋₁₎]`.head.copy().asInstanceOf[V]
    for (vector <- `[v₀v₁⋯v₍ₙ₋₁₎]`.tail) {
      μ⃑.add(vector)
    }
    μ⃑.scale(`¹/ₙ`)
  }).asInstanceOf[V]

}

// trait for Vector Companion Objects
trait VectorCompanion[ᵥ⃑ <: Vector] {
  def apply(values: VectorValues): Vector

  def blend(α: Double, v0: Vector, v1: Vector): Vector = (v0 * α) + (v1 * (1.0 - α))

  def average(`[v₁v₂⋯vₙ]`:Vector*): Vector = Vector.average[ᵥ⃑](`[v₁v₂⋯vₙ]`:_*)

  def average(`[v₀v₁⋯v₍ₙ₋₁₎]`: VECTORS): Vector = Vector.average(`[v₀v₁⋯v₍ₙ₋₁₎]`)

  def midpoint(v0: Vector, v1: Vector): Vector = blend(0.5, v0, v1)
}

trait Vector {

  def values: VectorValues

  def dimension: Int

  def component(i: Int): Double

  def magnitudeSquared(): Double

  def magnitude(): Double = Math.sqrt( magnitudeSquared() )

  def copy():Vector

  /**
   * For exotic vector formatting, provide lambdas for generating prefix, delimiter, and suffix.
   *
   * @param prefix maps a vector to a prefix.
   * @param delimiter maps vector element at index i to a delimiter.
   * @param suffix maps vector to a suffix.
   * @return
   */
  def dynamicCustomToString(
                            prefix: Vector => String,
                            delimiter:Int => String,
                            suffix: Vector => String,
                            sb:StringBuilder = new StringBuilder(),
                            numberFormatter:Double => String = (d:Double) => d.toString ):StringBuilder = {

    sb.append(prefix(this))
    for (i <- 0 until values.length - 1) {
      sb.append(numberFormatter(this.values(i)))
        .append(delimiter(i))
    }
    sb.append(numberFormatter(this.values(values.length - 1)))
      .append(suffix(this))
  }

  def customToString(
                prefix:String,
                delimiter:String,
                suffix:String,
                sb:StringBuilder = new StringBuilder(),
                numberFormatter:Double => String = (d:Double) => d.toString ):StringBuilder = {
    sb.append(prefix)
      .append(delimitedValues(delimiter, sb, numberFormatter))
      .append(suffix)
  }

  def commaSeparatedValues( sb:StringBuilder = new StringBuilder(),
                            numberFormatter:Double => String = (d:Double) => d.toString ):StringBuilder = delimitedValues(", ", sb, numberFormatter)

  def tabSeparatedValues( sb:StringBuilder = new StringBuilder(),
                          numberFormatter:Double => String = (d:Double) => d.toString ):StringBuilder = delimitedValues("\t", sb, numberFormatter)

  def delimitedValues( separator:String,
                       sb:StringBuilder = new StringBuilder(),
                       numberFormatter:Double => String = (d:Double) => d.toString ): StringBuilder = {
    sb.append(values.head)
    for (v <- values.tail) sb.append(separator).append(numberFormatter(v))
    sb
  }
}

case class UnsupportedVectorDimension(d:Int) extends Exception(
  s"Vector dimensions must exceed 1.  Cannot create a vector of dimension: $d"
)

case class MismatchedVectorDimensionsException(v0:Vector, v1:Vector) extends Exception(
  s"Operation undefined on vectors with different dimensions:\n\tdim($v0) = ${v0.dimension}\n\tdim($v1) = ${v1.dimension}"
)

case class ExtraDimensionalAccessException(v:Vector, ci: Int) extends Exception(
  s"Index: $ci exceeds dimensionality of Vector${v.dimension}: $v"
)

case class VectorNormalizationException(v:Vector) extends Exception(s"Can't normalize $v")