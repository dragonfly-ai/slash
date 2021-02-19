package ai.dragonfly.math.vector

import scala.scalajs.js


object VectorN {

  def fill(dimension: Int, value:Double): VectorN = new VectorN(Array.fill[Double](dimension)(value))

  def random(dimension: Int, maxNorm:Double = 1.0): VectorN = new VectorN(Array.fill[Double](dimension)(Math.random()*maxNorm))

  def midpoint(v0: VectorN, v1: VectorN): VectorN = {
    if (v0.dimension != v1.dimension) throw MismatchedVectorDimensionsException(
      s"midpoint undefined on vectors with different dimensions:\n" +
        s"dim($v0) = ${v0.dimension}\n" +
        s"dim($v1) = ${v1.dimension}"
    )
    val midpointValues = new Array[Double](v0.values.length)
    for (i <- 0 until v0.dimension) { midpointValues(i) = (v0(i) + v1(i)) / 2.0 }
    new VectorN(midpointValues)
  }

  def average(vectors: Array[VectorN]): Vector = {
    val l = vectors(0).dimension
    for(i <- 1 until vectors.length) {
      if (vectors(i).dimension != l) throw MismatchedVectorDimensionsException(
        s"average undefined on vectors with different dimensions:\n" +
          s"dim(${vectors(0)}}) = $l\n" +
          s"dim(${vectors(i)}) = ${vectors(i).dimension}"
      )
    }

    val avg = new VectorN(new Array[Double](l))
    for (vector <- vectors) {
      avg.add(vector)
    }
    avg.scale(1.0/l)
  }

}

/**
 * Created by clifton on 1/9/17.
 */

class VectorN(val values: Array[Double]) extends Vector {

  def this(vals: Double*) = this(vals.toArray[Double])

  def apply(i: Int): Double = values(i)

  override def clone(): VectorN = {
    val clonedValues = new Array[Double](values.length)
    for (i <- values.indices) clonedValues(i) = values(i)
    new VectorN(clonedValues)
  }

  def copy():VectorN = clone()

  def dimension: Int = values.length

  def divide(denominator: Double): VectorN = scale(1.0/denominator)

  def component(i: Int): Double = values(i)

  def magnitudeSquared(): Double = { var mag2 = 0.0; for (v <- values) mag2 = mag2 + (v*v); mag2 }

  def distanceSquaredTo(v0: Vector): Double = {
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

  def dot(v0: Vector): Double = {
    val v0Values = v0.values
    if (values.length != v0Values.length) throw MismatchedVectorDimensionsException(
      s"dot undefined on vectors with different dimensions:\n" +
        s"dim($this) = $dimension\n" +
        s"dim($v0) = ${v0.dimension}"
    ) else {
      var accumulator = 0.0
      for (i <- values.indices) {
        accumulator = accumulator + v0Values(i) * values(i)
      }
      accumulator
    }
  }


  def scale(scalar: Double): VectorN = {
    for (i <- values.indices) values(i) = values(i) * scalar
    this
  }

  def normalize(): VectorN = {
    val mag2 = magnitudeSquared()
    if (mag2 > 0.0) {
      val mag = Math.sqrt(mag2)
      for (i <- values.indices) values(i) = values(i) / mag
    }
    this
  }

  def round(): VectorN = {
    for (i <- values.indices) values(i) = Math.round(values(i)).toDouble
    this
  }

  def add(v0: Vector): VectorN = {
    val v0Values = v0.values
    if (values.length != v0Values.length) throw MismatchedVectorDimensionsException(
      s"add undefined on vectors with different dimensions:\n" +
        s"dim($this) = $dimension\n" +
        s"dim($v0) = ${v0.dimension}"
    ) else {
      for (i <- values.indices) values(i) = values(i) + v0Values(i)
      this
    }
  }

  def subtract(v0: Vector): VectorN = {
    val v0Values = v0.values
    if (values.length != v0Values.length) throw MismatchedVectorDimensionsException(
      s"subtract undefined on vectors with different dimensions:\n" +
        s"dim(${toString()}) = $dimension\n" +
        s"dim($v0) = ${v0.dimension}"
    ) else {
      for (i <- values.indices) values(i) = values(i) - v0Values(i)
      this
    }
  }

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