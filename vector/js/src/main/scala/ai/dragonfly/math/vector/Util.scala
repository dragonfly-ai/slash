package ai.dragonfly.math.vector

import scala.scalajs.js

/**
 * Created by clifton on 1/9/17.
 */
object Util {
  def toScalaArray(vectors: js.Array[VectorNCapabilities]): Array[VectorNCapabilities] = {
    val vArr: Array[VectorNCapabilities] = new Array[VectorNCapabilities](vectors.length)
    for (i <- 0 until vectors.length) vArr(i) = vectors(i)
    vArr
  }

  def toScalaArray(vectors: js.Array[VectorN]): Array[VectorN] = {
    val vArr: Array[VectorN] = new Array[VectorN](vectors.length)
    for (i <- 0 until vectors.length) vArr(i) = vectors(i)
    vArr
  }

  def toScalaArray(values: js.Array[Double]): Array[Double] = {
    val dArr: Array[Double] = new Array[Double](values.length)
    for (i <- 0 until values.length) dArr(i) = values(i)
    dArr
  }
}
