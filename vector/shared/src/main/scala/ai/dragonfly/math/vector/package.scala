package ai.dragonfly.math

import scala.quoted.Type
import bridge.array.*

package object vector {

  inline def dimensionCheck(supplied:Int, required: Int): Unit = {
    if (supplied != required) throw UnsupportedVectorDimension(supplied, required)
  }

  inline def dimensionCheck(values:ARRAY[Double], requiredDimension: Int): ARRAY[Double] = {
    dimensionCheck(values.length, requiredDimension)
    values
  }

}

