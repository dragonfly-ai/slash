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

  
  def midpoint(v0: Vector, v1: Vector): Vector = average(v0, v1)

  def average(vectors:Vector*): Vector = {
    val avg:Vector = vectors.head.copy()
    for (vector <- vectors.tail) {
      avg.add(vector)
    }
    avg.scale(1.0/vectors.size)
  }

  
  def average(vectors: native.VECTORS): Vector = {
    val avg:Vector = vectors.head.copy()
    for (vector <- vectors.tail) {
      avg.add(vector)
    }
    avg.scale(1.0/vectors.size)
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

  def distanceSquaredTo(v0: Vector): Double

  def distanceTo(v0: Vector): Double = Math.sqrt(distanceSquaredTo(v0))

  def dot(v0: Vector): Double

  def round(): Vector

  // in place operators
  def scale(scalar: Double): Vector

  def divide(denominator: Double): Vector = scale(1.0 / denominator)

  def add(v0: Vector): Vector

  def subtract(v0: Vector): Vector

  def center(vectors: VECTORS): VECTORS = {
    for (v: Vector <- vectors) v.subtract(this)
    vectors
  }

  def copy(): Vector

  // copy operators
  def *(scalar:Double) = this.copy().scale(scalar)
  def /(scalar:Double) = this.copy().divide(scalar)

  def +(v: Vector) = this.copy().add(v)
  def -(v: Vector) = this.copy().subtract(v)

  def csv: String = {
    val sb:StringBuilder = new StringBuilder()
    sb.append(values.head)
    for (v <- values.tail) sb.append(", ").append(v)
    sb.toString
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