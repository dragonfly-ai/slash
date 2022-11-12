package ai.dragonfly.math

import scala.quoted.Type
import narr.*

package object vector {

  inline def dimensionCheck(supplied:Int, required: Int): Unit = {
    if (supplied != required) throw UnsupportedVectorDimension(supplied, required)
  }

  inline def dimensionCheck(values:NArray[Double], requiredDimension: Int): NArray[Double] = {
    dimensionCheck(values.length, requiredDimension)
    values
  }

}

