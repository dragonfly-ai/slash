package ai.dragonfly.math.vector

import ai.dragonfly.math.example.Demonstrable

import ai.dragonfly.math.*
import Random.*
import ai.dragonfly.math.vector.Vector.*

/**
 * Created by clifton on 1/9/17.
 */

object VectorN extends VectorCompanion[VectorN] with Demonstrable {

  def apply(values:Double*):VectorN = new VectorN(VectorValues(values:_*))

  override def apply(values:VectorValues): VectorN = new VectorN(values)

  override def validDimension(dimension: Int): Boolean = dimension > 4

  def fill(dimension:Int, d:Double):VectorN = {
    given dim:Int = dimension
    super.fill(d)
  }
  def tabulate(dimension:Int, f: Int => Double):VectorN = {
    given dim:Int = dimension
    super.tabulate(f)
  }

  //def random(dimension:Int, maxNorm:Double = 1.0): VectorN = new VectorN(VectorValues.tabulate(dimension)((i:Int) => maxNorm * Math.random()))

  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    import Console.{GREEN, RED, RESET, YELLOW, UNDERLINED, RED_B}

    sb.append("\n\nVectorN.fill(9, 0)").append(VectorN.fill(9, 0))

    val rvn:VectorN = defaultRandom.nextVector(42, 777.777).asInstanceOf[VectorN]
    sb.append("\n\nval rvn:VectorN = VectorN.random(42, 777)")
      .append("\n\trvn.toString: ")
      .append("\n\t\t").append(rvn)
      .append("\n\trvn.exhaustiveToString(): ")
      .append("\n\t\t").append(rvn.exhaustiveToString())
      .append("\n\trvn.exhaustiveToString(numberFormatter = (d:Double) => \"%7.3f\").format(d)): ")
      .append("\n\t\t").append(rvn.exhaustiveToString(numberFormatter = (d:Double) => "%7.3f".format(d)))
      .append("\n\trvn.indexedExhaustiveToString(): ")
      .append("\n\t\t").append(rvn.indexedExhaustiveToString(numberFormatter = (d:Double) => "%7.3f".format(d)))
      .append("\n\trvn.indexedExhaustiveToString(numberFormatter = (d:Double) => \"%7.3f\".format(d)): ")
      .append("\n\t\t").append(rvn.indexedExhaustiveToString(numberFormatter = (d:Double) => "%7.3f".format(d)))


    sb.append("\n\nVector.midpoint(new VectorN(1.0, 2.0, 3.0, 4.0, 5.0), VectorN(5.0, 4.0, 3.0, 2.0, 1.0))\n\t")
      .append(Vector.midpoint(VectorN(1.0, 2.0, 3.0, 4.0, 5.0), VectorN(5.0, 4.0, 3.0, 2.0, 1.0)).toString)
      .append("\n")

  }

  override def name: String = "VectorN"

}

class VectorN(override val values:VectorValues) extends Vector[VectorN] {

  override inline def copy():VectorN = {
    val cp:VectorValues = new VectorValues(values.length)
    for (i <- values.indices) cp(i) = values(i)
    new VectorN(cp)
  }

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