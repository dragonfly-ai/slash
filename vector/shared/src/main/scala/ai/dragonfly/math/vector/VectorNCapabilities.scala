package ai.dragonfly.math.vector

import scala.scalajs.js.annotation.JSExport

/**
 * Created by clifton on 1/9/17.
 */
trait VectorNCapabilities {

  def values: Array[Double]

  @JSExport def dimension: Int = values.length

  @JSExport def component(i: Int): Double = values(i)

  @JSExport def magnitude(): Double = Math.sqrt( magnitudeSquared() )

  @JSExport def magnitudeSquared(): Double = { var mag2 = 0.0; for (v <- values) mag2 = mag2 + (v*v); mag2 }


  @JSExport def distanceSquaredTo(v0: VectorNCapabilities): Double = {
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

  @JSExport def distanceTo(v0: VectorNCapabilities): Double = Math.sqrt(distanceSquaredTo(v0))



  @JSExport def dot(v0: VectorNCapabilities): Double = {
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


  @JSExport def scale(scalar: Double): VectorNCapabilities = {
    for (i <- 0 until values.length) values(i) = values(i) * scalar
    this
  }

  @JSExport def divide(denominator: Double): VectorNCapabilities = scale(1.0/denominator)


  @JSExport def normalize(): VectorNCapabilities = {
    val mag2 = magnitudeSquared()
    if (mag2 > 0.0) {
      val mag = Math.sqrt(mag2)
      for (i <- 0 until values.length) values(i) = values(i) / mag
    }
    this
  }


  @JSExport def add(v0: VectorNCapabilities): VectorNCapabilities = {
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

  @JSExport def subtract(v0: VectorNCapabilities): VectorNCapabilities = {
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


  @JSExport def center(vectors: Array[VectorNCapabilities]): Array[VectorNCapabilities] = {
    for (v: VectorNCapabilities <- vectors) v.subtract(this)
    vectors
  }


  @JSExport override def toString(): String = {
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
