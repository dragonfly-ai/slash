package ai.dragonfly.math.vector

import scala.scalajs.js.annotation.JSExport

/**
 * Created by clifton on 1/8/17.
 */

trait UtilVectorN {

  @JSExport def fill(dimension: Int, value:Double): VectorN = {
    new VectorN(Array.fill[Double](dimension)(value))
  }

  @JSExport def random(dimension: Int, maxNorm:Double = 1.0): VectorN = {
    new VectorN(Array.fill[Double](dimension)(Math.random()*maxNorm))
  }

  @JSExport def midpoint(v0: VectorN, v1: VectorN): VectorN = {
    if (v0.dimension != v1.dimension) throw MismatchedVectorDimensionsException(
      s"midpoint undefined on vectors with different dimensions:\n" +
        s"dim($v0) = ${v0.dimension}\n" +
        s"dim($v1) = ${v1.dimension}"
    )
    val midpointValues = new Array[Double](v0.values.length)
    for (i <- 0 until v0.dimension) { midpointValues(i) = (v0.values(i) + v1.values(i)) / 2.0 }
    new VectorN(midpointValues)
  }

  @JSExport def average(vectors: Array[VectorN]): VectorNCapabilities = {
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

case class MismatchedVectorDimensionsException(message: String) extends Exception(message)
