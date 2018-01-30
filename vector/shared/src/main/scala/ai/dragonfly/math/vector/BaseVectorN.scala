package ai.dragonfly.math.vector

import scala.scalajs.js.annotation.JSExport

/**
 * Created by clifton on 1/10/17.
 */
trait BaseVectorN extends Vector {

  override def values: Array[Double]

  override def dimension: Int = values.length

  override def divide(denominator: Double): Vector = scale(1.0/denominator)

  override def component(i: Int): Double = values(i)


  override def magnitudeSquared(): Double = { var mag2 = 0.0; for (v <- values) mag2 = mag2 + (v*v); mag2 }


  override def distanceSquaredTo(v0: Vector): Double = {
    val v0Values = v0.values
    if (values.length != v0Values.length) throw MismatchedVectorDimensionsException(
      s"distanceSquaredTo undefined on vectors with different dimensions:\n" +
        s"dim($this) = ${dimension}\n" +
        s"dim($v0) = ${v0.dimension}"
    ) else {
      var distance = 0.0
      val v0Values = v0.values
      for ( i <- 0 until v0.dimension ) {
        val delta = values(i) - v0Values(i)
        distance = distance + delta * delta
      }
      distance
    }

  }

  override def dot(v0: Vector): Double = {
    val v0Values = v0.values
    if (values.length != v0Values.length) throw MismatchedVectorDimensionsException(
      s"dot undefined on vectors with different dimensions:\n" +
        s"dim($this) = ${dimension}\n" +
        s"dim($v0) = ${v0.dimension}"
    ) else {
      var accumulator = 0.0
      for (i <- 0 until values.length) {
        accumulator = accumulator + v0Values(i) * values(i)
      }
      accumulator
    }
  }


  override def scale(scalar: Double): Vector = {
    for (i <- 0 until values.length) values(i) = values(i) * scalar
    this
  }

  override def normalize(): Vector = {
    val mag2 = magnitudeSquared()
    if (mag2 > 0.0) {
      val mag = Math.sqrt(mag2)
      for (i <- 0 until values.length) values(i) = values(i) / mag
    }
    this
  }


  override def add(v0: Vector): Vector = {
    val v0Values = v0.values
    if (values.length != v0Values.length) throw MismatchedVectorDimensionsException(
      s"add undefined on vectors with different dimensions:\n" +
        s"dim($this) = ${dimension}\n" +
        s"dim($v0) = ${v0.dimension}"
    ) else {
      for (i <- 0 until values.length) values(i) = values(i) + v0Values(i)
      this
    }
  }

  override def subtract(v0: Vector): Vector = {
    val v0Values = v0.values
    if (values.length != v0Values.length) throw MismatchedVectorDimensionsException(
      s"subtract undefined on vectors with different dimensions:\n" +
        s"dim($toString) = $dimension\n" +
        s"dim($v0) = ${v0.dimension}"
    ) else {
      for (i <- 0 until values.length) values(i) = values(i) - v0Values(i)
      this
    }
  }

  override def copy(): Vector


  override def toString(): String = {
    val sb = new StringBuilder(dimension * 10)
    sb.append(s"[${values(0)}")
    if (dimension > 16) {
      for (i <- 1 until 8) sb.append(s",${values(i)}")
      sb.append(", ... ")
      for (i <- dimension - 9 until dimension) sb.append(s",${values(i)}")
    } else {
      for (i <- 1 until dimension) sb.append(s",${values(i)}")
    }
    sb.append("]")
    sb.toString
  }
}
