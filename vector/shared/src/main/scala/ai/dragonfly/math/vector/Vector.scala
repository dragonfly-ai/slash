package ai.dragonfly.math.vector

import ai.dragonfly.math.{Euclidean, squareInPlace}

import scala.scalajs.js
import ai.dragonfly.math.vector.*

import scala.quoted.Type
import scala.util.Random

object Vector {

  def fill(dimension:Int)(d: Double): Vector = apply(VectorValues.fill(dimension)(d))

  def tabulate(dimension:Int)(f: Int => Double): Vector = apply(VectorValues.tabulate(dimension)(f))

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

  def midpoint[V <: VectorOps[V]](v0: V, v1: V):V = ((v0 + v1) * 0.5)

  def mean[V <: VectorOps[V]](`[v₁v₂⋯vₙ]`:V*):V = {
    val μ:V = `[v₁v₂⋯vₙ]`.head.copy()
    for (v <- `[v₁v₂⋯vₙ]`.tail) {
      μ += v
    }
    μ /= `[v₁v₂⋯vₙ]`.size
  }

  def mean[V <: VectorOps[V]](`[v₀v₁⋯v₍ₙ₋₁₎]`: VECTORS):V = {
    val μ:V = `[v₀v₁⋯v₍ₙ₋₁₎]`(0).asInstanceOf[V].copy()
    for (i <- 1 to `[v₀v₁⋯v₍ₙ₋₁₎]`.length) {
      μ += μ.recognize(`[v₀v₁⋯v₍ₙ₋₁₎]`(i))
    }
    μ /= `[v₀v₁⋯v₍ₙ₋₁₎]`.length
  }

}

// trait for Vector Companion Objects
trait VectorCompanion[V <: Vector with VectorOps[V]] {

  def validDimension(dimension:Int):Boolean

  def apply(values: VectorValues): V

  protected def fill(value: Double)(using dimension:Int):V = apply(VectorValues.fill(dimension)(value))

  protected def tabulate(f: Int => Double)(using dimension:Int):V = apply(VectorValues.tabulate(dimension)(f))

  def blend(alpha: Double, v0: V, v1: V):V = ((v0 * alpha) + (v1 * (1.0 - alpha))).asInstanceOf[V]

  def mean(`[v₁v₂⋯vₙ]`:V*):V = Vector.mean[V](`[v₁v₂⋯vₙ]`:_*)

  def mean(`[v₀v₁⋯v₍ₙ₋₁₎]`: VECTORS):V = Vector.mean(`[v₀v₁⋯v₍ₙ₋₁₎]`).asInstanceOf[V]

  def midpoint(v0: V, v1: V):V = blend(0.5, v0, v1)
}

trait VectorOps[V <: Vector] extends Vector with Euclidean[V with VectorOps[V]] {

  type VEC = V with VectorOps[V]

  // abstract
  def round():VEC

  def scale(scalar: Double):VEC
  def divide(divisor: Double):VEC

  def dot(v0:VEC): Double

  def add(v0:VEC):VEC

  def subtract(v0:VEC):VEC

  def recognize(v: Vector):VEC

  // implemented
  def magnitudeSquared:Double = euclideanNormSquared
  def magnitude:Double = euclideanNorm

  def normalize():VEC = {
    val m2:Double = this.euclideanNorm
    if (m2 > 0.0) divide(Math.sqrt(m2))
    else throw VectorNormalizationException(this)
  }

  def +=(v0:VEC):VEC = add(v0)
  def -=(v0:VEC):VEC = subtract(v0)
  def *= (scalar: Double):VEC =  scale(scalar)
  def /= (divisor: Double):VEC = divide(divisor)

  def copy():VEC

  // copy operators
  def *(scalar:Double):VEC = copy().scale(scalar)
  def /(divisor:Double):VEC = copy().divide(divisor)

  def +(v0:VEC):VEC = copy().add(v0)
  def -(v0:VEC ):VEC = copy().subtract(v0)

}

trait Vector {

  def values: VectorValues

  def dimension: Int

  def component(i: Int): Double


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