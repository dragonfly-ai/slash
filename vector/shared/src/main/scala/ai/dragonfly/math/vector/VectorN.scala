package ai.dragonfly.math.vector

import ai.dragonfly.math.*
import Random.*
import ai.dragonfly.math.vector.Vector.*
import narr.*

/**
 * Created by clifton on 1/9/17.
 */

object VectorN extends VectorCompanion[VectorN] {

  def apply(values:Double*):VectorN = new VectorN(NArray[Double](values:_*))

  override def apply(values:NArray[Double]): VectorN = {
    if ( validDimension(values.length) ) new VectorN(values)
    else throw UnsupportedVectorDimension(values.length, 5)
  }

  override inline def validDimension(dimension: Int): Boolean = dimension > 4

  def fill(dimension:Int, d:Double):VectorN = {
    given dim:Int = dimension
    super.fill(d)
  }

  def tabulate(dimension:Int, f: Int => Double):VectorN = {
    given dim:Int = dimension
    super.tabulate(f)
  }

}

class VectorN private (override val values:NArray[Double]) extends Vector {

  type VEC = VectorN

  override def copy():VEC = VectorN( NArray.tabulate[Double](values.length)( (i:Int) => values(i) ) )


  import unicode.*

  def indexedExhaustiveToString(sb:StringBuilder = new StringBuilder(), numberFormatter:Double => String = d => d.toString):StringBuilder = {
    dynamicCustomToString(
      (v:VectorData) => s"《${exalt(this.dimension)}↗〉",
      (i:Int) => abase(i) + " ",
      (v:VectorData) => "〉",
      sb,
      numberFormatter
    )
  }

  def exhaustiveToString(sb:StringBuilder = new StringBuilder(), numberFormatter:Double => String = d => d.toString):StringBuilder = {
    customToString(s"《${exalt(dimension)}↗〉", ", ", "〉")
  }

  /**
   * for vectors of dimension > 10, VectorN.toString only prints the first and last 4 elements of the vector.
   * If you want to export vector data, use commaSeparatedValues, tabSeparatedValues, or delimitedValues.
   * If you want to print the entire vector in a human readable way, use exhaustiveToString, customToString, or dynamicCustomToString.
   *
   * @return a human readible string value to represent this vector.
   */
  override def toString:String = {
    val sb = new StringBuilder("《ⁿ↗")
    if (dimension > 10) {
      sb.append(s" ✂〉${values(0)}")
      for (i <- 1 until 4) sb.append(s", ${values(i)}")
      sb.append(", ⋯")
      for (i <- dimension - 4 until dimension) sb.append(s", ${values(i)}")
    } else {
      sb.append(s"〉${values(0)}")
      for (i <- 1 until dimension) sb.append(s", ${values(i)}")
    }
    sb.append("〉")
    sb.toString
  }



}