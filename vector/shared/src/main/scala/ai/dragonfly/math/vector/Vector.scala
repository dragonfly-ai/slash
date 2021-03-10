package ai.dragonfly.math.vector

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExportAll
object Vector {

  @JSExport("apply")
  def apply(d:Double*): Vector = d.size match {
    case 2 => Vector2(d(0), d(1))
    case 3 => Vector3(d(0), d(1), d(2))
    case n if n > 3 => VectorN(VectorValues(d:_*))
  }

  def fill(dimension: Int, value:Double): Vector = dimension match {
    case 2 => Vector2(value, value)
    case 3 => Vector3(value, value, value)
    case n if n > 3 => VectorN(VectorValues.fill(dimension)((_:Int) => value))
  }

  def random(dimension: Int, maxNorm:Double = 1.0): Vector = {
    def r(i:Int = 0): Double = Math.random() * maxNorm
    dimension match {
      case 2 => Vector2(r(), r())
      case 3 => Vector3(r(), r(), r())
      case n if n > 3 => new VectorN(VectorValues.fill(dimension)(r))
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

@JSExportAll
trait Vector {

  @inline def values: native.VectorValues

  @inline def dimension: Int

  @inline def component(i: Int): Double

  @inline def magnitude(): Double = Math.sqrt( magnitudeSquared() )

  @inline def magnitudeSquared(): Double


  @inline def distanceSquaredTo(v0: Vector): Double

  @inline def distanceTo(v0: Vector): Double = Math.sqrt(distanceSquaredTo(v0))


  @inline def dot(v0: Vector): Double

  @inline def scale(scalar: Double): Vector

  @inline def divide(denominator: Double): Vector


  @inline def normalize(): Vector

  @inline def round(): Vector


  @inline def add(v0: Vector): Vector

  @inline def subtract(v0: Vector): Vector

  @inline def center(vectors: native.VECTORS): native.VECTORS = {
    for (v: Vector <- vectors) v.subtract(this)
    vectors
  }

  @inline def copy(): Vector

}


case class MismatchedVectorDimensionsException(v0:Vector, v1:Vector) extends Exception(
  s"Operation undefined on vectors with different dimensions:\n\tdim($v0) = ${v0.dimension}\n\tdim($v1) = ${v1.dimension}"
)