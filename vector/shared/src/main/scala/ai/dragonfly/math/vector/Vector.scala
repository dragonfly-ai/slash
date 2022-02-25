package ai.dragonfly.math.vector

import scala.scalajs.js

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
  
  def fill(dimension: Int, value:Double): Vector = dimension match {
    case dim if dim < 2 => throw UnsupportedVectorDimension(dim)
    case 2 => Vector2(value, value)
    case 3 => Vector3(value, value, value)
    case 4 => Vector4(value, value, value, value)
    case _ => VectorN(VectorValues.fill(dimension)((_:Int) => value))
  }

  def fill(dimension:Int)(f: Int => Double):Vector = dimension match {
    case dim if dim < 2 => throw UnsupportedVectorDimension(dim)
    case 2 => Vector2(f(0), f(1))
    case 3 => Vector3(f(0), f(1), f(2))
    case 4 => Vector4(f(0), f(1), f(2), f(3))
    case _ => VectorN(VectorValues.fill(dimension)(f))
  }

  def random(dimension: Int, maxNorm:Double = 1.0): Vector = {
    def r(i:Int = 0): Double = Math.random() * maxNorm
    dimension match {
      case dim if dim < 2 => throw UnsupportedVectorDimension(dim)
      case 2 => Vector2(r(), r())
      case 3 => Vector3(r(), r(), r())
      case 4 => Vector4(r(), r(), r(), r())
      case _ => new VectorN(VectorValues.fill(dimension)(r))
    }
  }

  def midpoint(`v⃑₀`: Vector, `v⃑₁`: Vector): Vector = {
    //average(`v⃑₀`, `v⃑₁`)
    (`v⃑₀` + `v⃑₁`) * 0.5
  }

  def average(`[v⃑₁v⃑₂⋯v⃑ₙ]`:Vector*): Vector = {
    val μ⃑:Vector = `[v⃑₁v⃑₂⋯v⃑ₙ]`.head.copy()
    for (vector <- `[v⃑₁v⃑₂⋯v⃑ₙ]`.tail) {
      μ⃑.add(vector)
    }
    μ⃑.scale(1.0/`[v⃑₁v⃑₂⋯v⃑ₙ]`.size)
  }

  
  def average(`[v⃑₀v⃑₁⋯v⃑₍ₙ₋₁₎]`: VECTORS): Vector = {
    val `¹/ₙ`:Double = 1.0 / `[v⃑₀v⃑₁⋯v⃑₍ₙ₋₁₎]`.length
    val μ⃑:Vector = `[v⃑₀v⃑₁⋯v⃑₍ₙ₋₁₎]`.head.copy()
    for (vector <- `[v⃑₀v⃑₁⋯v⃑₍ₙ₋₁₎]`.tail) {
      μ⃑.add(vector)
    }
    μ⃑.scale(`¹/ₙ`)
  }
}

/**
 * Created by clifton on 1/9/17.
 */


trait Vector {

  def values: VectorValues

  def dimension: Int

  def component(i: Int): Double

  def magnitudeSquared(): Double

  def magnitude(): Double = Math.sqrt( magnitudeSquared() )

  def normalize(): Vector = {
    val m2:Double = magnitudeSquared()
    if (m2 > 0.0) divide(Math.sqrt(m2))
    else throw VectorNormalizationException(this)
  }

  def distanceSquaredTo(v⃑: Vector): Double

  def distanceTo(v⃑: Vector): Double = Math.sqrt(distanceSquaredTo(v⃑))

  def dot(v⃑: Vector): Double

  def round(): Vector

  // in place operators
  def scale(scalar: Double): Vector

  def divide(denominator: Double): Vector = scale(1.0 / denominator)

  def add(v⃑: Vector): Vector

  def subtract(v⃑: Vector): Vector

  def center(vectors: VECTORS): VECTORS = {
    for (v⃑: Vector <- vectors) v⃑.subtract(this)
    vectors
  }

  def copy(): Vector

  // copy operators
  def *(scalar:Double) = this.copy().scale(scalar)
  def /(scalar:Double) = this.copy().divide(scalar)

  def +(v⃑: Vector) = this.copy().add(v⃑)
  def -(v⃑: Vector) = this.copy().subtract(v⃑)

  /**
   * For exotic vector formatting, provide lambdas for generating prefix, delimiter, and suffix.
   *
   * @param prefix maps a vector to a prefix.
   * @param delimiter maps vector element at index i to a delimiter.
   * @param suffix maps vector to a suffix.
   * @return
   */
  def dynamicCustomToString(
                            prefix:Vector => String,
                            delimiter:Int => String,
                            suffix:Vector => String,
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

case class MismatchedVectorDimensionsException(`v⃑₀`: Vector, `v⃑₁`: Vector) extends Exception(
  s"Operation undefined on vectors with different dimensions:\n\tdim(${`v⃑₀`}) = ${`v⃑₀`.dimension}\n\tdim(${`v⃑₁`}) = ${`v⃑₁`.dimension}"
)

case class ExtraDimensionalAccessException(v⃑:Vector, ci: Int) extends Exception(
  s"Index: $ci exceeds dimensionality of Vector${v⃑.dimension}: $v⃑"
)

case class VectorNormalizationException(v⃑:Vector) extends Exception(s"Can't normalize $v⃑")