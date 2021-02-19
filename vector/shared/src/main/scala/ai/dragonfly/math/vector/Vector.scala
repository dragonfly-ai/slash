package ai.dragonfly.math.vector

import scala.scalajs.js.annotation.JSExport

/**
 * Created by clifton on 1/9/17.
 */
trait Vector extends native.Vector {

  def values: Array[Double]

  @JSExport def dimension: Int

  @JSExport def component(i: Int): Double

  @JSExport def magnitude(): Double = Math.sqrt( magnitudeSquared() )

  @JSExport def magnitudeSquared(): Double


  @JSExport def distanceSquaredTo(v0: Vector): Double

  @JSExport def distanceTo(v0: Vector): Double = Math.sqrt(distanceSquaredTo(v0))


  @JSExport def dot(v0: Vector): Double

  @JSExport def scale(scalar: Double): Vector

  @JSExport def divide(denominator: Double): Vector


  @JSExport def normalize(): Vector

  @JSExport def round(): Vector


  @JSExport def add(v0: Vector): Vector

  @JSExport def subtract(v0: Vector): Vector

  @JSExport def center(vectors: Array[Vector]): Array[Vector] = {
    for (v: Vector <- vectors) v.subtract(this)
    vectors
  }

  @JSExport def copy(): Vector

}

case class MismatchedVectorDimensionsException(message: String) extends Exception(message)