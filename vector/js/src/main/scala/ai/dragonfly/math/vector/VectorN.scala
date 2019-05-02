package ai.dragonfly.math.vector

import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel}

object VectorN {

  @JSExport def fill(dimension: Int, value:Double): VectorN = {
    val arr = new js.Array[Double](dimension)
    for (i <- arr.indices) arr(i) = value
    new VectorN(arr)
  }

  @JSExport def random(dimension: Int, maxNorm:Double = 1.0): VectorN = {
    val arr = new js.Array[Double](dimension)
    for (i <- arr.indices) arr(i) = Math.random()*maxNorm
    new VectorN(arr)
  }

  @JSExport def midpoint(v0: VectorN, v1: VectorN): VectorN = {
    if (v0.dimension != v1.dimension) throw MismatchedVectorDimensionsException(
      s"midpoint undefined on vectors with different dimensions:\n" +
        s"dim($v0) = ${v0.dimension}\n" +
        s"dim($v1) = ${v1.dimension}"
    )
    val midpointValues = new js.Array[Double](v0.values.length)
    for (i <- 0 until v0.dimension) { midpointValues(i) = (v0(i) + v1(i)) / 2.0 }
    new VectorN(midpointValues)
  }

  @JSExport def average(vectors: Array[VectorN]): Vector = average(vectors.toJSArray)

  @JSExport def average(vectors: js.Array[VectorN]): Vector = {
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
@JSExportTopLevel("ai.dragonfly.math.vector.VectorN") @JSExportAll
class VectorN(override val jsValues: js.Array[Double]) extends Vector {

  @JSExportTopLevel("ai.dragonfly.math.vector.VectorN") def this(vals: Double*) = this(vals.toJSArray)

  @JSExportTopLevel("ai.dragonfly.math.vector.VectorN") def this(values: Array[Double]) { this(values.toJSArray) }

  // dont' call this method.
  @JSExport def values: Array[Double] = jsValues.toArray

  @JSExport("apply") def apply(i: Int): Double = jsValues(i)

  @JSExport override def clone(): VectorN = {
    val clonedValues = new Array[Double](jsValues.length)
    for (i <- jsValues.indices) clonedValues(i) = jsValues(i)
    new VectorN(clonedValues)
  }

  @JSExport def copy = clone()

  def dimension: Int = jsValues.length

  def divide(denominator: Double): VectorN = scale(1.0/denominator)

  def component(i: Int): Double = jsValues(i)

  def magnitudeSquared(): Double = { var mag2 = 0.0; for (v <- jsValues) mag2 = mag2 + (v*v); mag2 }

  def distanceSquaredTo(v0: Vector): Double = {
    val v0Values = v0.values
    if (jsValues.length != v0Values.length) throw MismatchedVectorDimensionsException(
      s"distanceSquaredTo undefined on vectors with different dimensions:\n" +
        s"dim($this) = ${dimension}\n" +
        s"dim($v0) = ${v0.dimension}"
    ) else {
      var distance = 0.0
      val v0Values = v0.values
      for ( i <- 0 until v0.dimension ) {
        val delta = jsValues(i) - v0Values(i)
        distance = distance + delta * delta
      }
      distance
    }
  }

  def dot(v0: Vector): Double = {
    val v0Values = v0.values
    if (jsValues.length != v0Values.length) throw MismatchedVectorDimensionsException(
      s"dot undefined on vectors with different dimensions:\n" +
        s"dim($this) = $dimension\n" +
        s"dim($v0) = ${v0.dimension}"
    ) else {
      var accumulator = 0.0
      for (i <- jsValues.indices) {
        accumulator = accumulator + v0Values(i) * jsValues(i)
      }
      accumulator
    }
  }

  def scale(scalar: Double): VectorN = {
    for (i <- jsValues.indices) jsValues(i) = jsValues(i) * scalar
    this
  }

  def normalize(): VectorN = {
    val mag2 = magnitudeSquared()
    if (mag2 > 0.0) {
      val mag = Math.sqrt(mag2)
      for (i <- jsValues.indices) jsValues(i) = jsValues(i) / mag
    }
    this
  }

  def round(): VectorN = {
    for (i <- jsValues.indices) jsValues(i) = Math.round(jsValues(i))
    this
  }

  def add(v0: Vector): VectorN = {
    val v0Values = v0.values
    if (jsValues.length != v0Values.length) throw MismatchedVectorDimensionsException(
      s"add undefined on vectors with different dimensions:\n" +
        s"dim($this) = $dimension\n" +
        s"dim($v0) = ${v0.dimension}"
    ) else {
      for (i <- jsValues.indices) jsValues(i) = jsValues(i) + v0Values(i)
      this
    }
  }

  def subtract(v0: Vector): VectorN = {
    val v0Values = v0.values
    if (jsValues.length != v0Values.length) throw MismatchedVectorDimensionsException(
      s"subtract undefined on vectors with different dimensions:\n" +
        s"dim(${toString()}) = $dimension\n" +
        s"dim($v0) = ${v0.dimension}"
    ) else {
      for (i <- jsValues.indices) jsValues(i) = jsValues(i) - v0Values(i)
      this
    }
  }

  override def toString(): String = {
    val sb = new StringBuilder(dimension * 10)
    sb.append(s"[${jsValues(0)}")
    if (dimension > 16) {
      for (i <- 1 until 8) sb.append(s",${jsValues(i)}")
      sb.append(", ... ")
      for (i <- dimension - 9 until dimension) sb.append(s",${jsValues(i)}")
    } else {
      for (i <- 1 until dimension) sb.append(s",${jsValues(i)}")
    }
    sb.append("]")
    sb.toString
  }

}
