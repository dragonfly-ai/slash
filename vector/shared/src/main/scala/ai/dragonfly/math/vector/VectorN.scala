package ai.dragonfly.math.vector

import ai.dragonfly.math.examples.Demonstrable

import ai.dragonfly.math.*

/**
 * Created by clifton on 1/9/17.
 */

object VectorN extends VectorCompanion[VectorN] with Demonstrable {

  def apply(values:Double*):VectorN = new VectorN(VectorValues(values:_*))

  override def apply(values:VectorValues): VectorN = new VectorN(values)

  def fill(dimension:Int, value:Double): VectorN = new VectorN(VectorValues.fill(dimension)((d:Int) => value))
  def fill(dimension:Int, f: Int => Double):VectorN = new VectorN(VectorValues.fill(dimension)(f))

  def random(dimension:Int, maxNorm:Double = 1.0): VectorN = new VectorN(VectorValues.fill(dimension)((d:Int) => maxNorm * Math.random()))

  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    import Console.{GREEN, RED, RESET, YELLOW, UNDERLINED, RED_B}

    sb.append("\n\nVectorN.fill(9, 0)").append(VectorN.fill(9, 0))

    val r:VectorN = VectorN.random(42, 777.777)
    sb.append("\n\nval r:VectorN = VectorN.random(42, 777)")
      .append("\n\tr.toString: ")
      .append("\n\t\t").append(r)
      .append("\n\tr.exhaustiveToString(): ")
      .append("\n\t\t").append(r.exhaustiveToString())
      .append("\n\tr.exhaustiveToString(numberFormatter = (d:Double) => \"%7.3f\").format(d)): ")
      .append("\n\t\t").append(r.exhaustiveToString(numberFormatter = (d:Double) => "%7.3f".format(d)))
      .append("\n\tr.indexedExhaustiveToString(): ")
      .append("\n\t\t").append(r.indexedExhaustiveToString(numberFormatter = (d:Double) => "%7.3f".format(d)))
      .append("\n\tr.indexedExhaustiveToString(numberFormatter = (d:Double) => \"%7.3f\".format(d)): ")
      .append("\n\t\t").append(r.indexedExhaustiveToString(numberFormatter = (d:Double) => "%7.3f".format(d)))


    sb.append("\n\nVector.midpoint(new VectorN(1.0, 2.0, 3.0, 4.0, 5.0), VectorN(5.0, 4.0, 3.0, 2.0, 1.0))\n\t")
      .append(Vector.midpoint(VectorN(1.0, 2.0, 3.0, 4.0, 5.0), VectorN(5.0, 4.0, 3.0, 2.0, 1.0)).toString)
      .append("\n")

  }

  override def name: String = "VectorN"

}

class VectorN(override val values:VectorValues) extends Vector {


  override def dimension: Int = values.length

  override def component(i: Int): Double = try {
    values(i)
  } catch {
    case aioobe:ArrayIndexOutOfBoundsException => throw ExtraDimensionalAccessException(this, i)
  }

  override def magnitudeSquared(): Double = { var mag2 = 0.0; for (v:Double <- values) mag2 = mag2 + (v*v); mag2 }

  def distanceSquaredTo(v: Vector): Double = {
    if (values.length != v.values.length) throw MismatchedVectorDimensionsException(this, v)
    else {
      var distance = 0.0
      for ( i <- 0 until v.dimension) {
        val delta = values(i) - v.values(i)
        distance = distance + delta * delta
      }
      distance
    }
  }


  def normalize():VectorN = {
    val m2:Double = magnitudeSquared()
    if (m2 > 0.0) divide(Math.sqrt(m2))
    else throw VectorNormalizationException(this)
  }


  def dot(v: VectorN): Double = {
    if (values.length != v.values.length) throw MismatchedVectorDimensionsException(this, v)
    else {
      var accumulator = 0.0
      for (i <- values.indices) {
        accumulator = accumulator + v.values(i) * values(i)
      }
      accumulator
    }
  }

  def += : (VectorN => VectorN) = add
  def -= : (VectorN => VectorN) = subtract
  def *= : (Double => VectorN) = scale
  def /= : (Double => VectorN) = divide

  def scale(scalar: Double): VectorN = {
    for (i <- values.indices) values(i) = values(i) * scalar
    this
  }

  def divide(divisor: Double): VectorN = scale(1 / divisor)

  def round(): VectorN = {
    for (i <- values.indices) values(i) = Math.round(values(i)).toDouble
    this
  }

  def add(v: VectorN): VectorN = {
    if (values.length != v.values.length) throw MismatchedVectorDimensionsException(this, v)
    else {
      for (i <- values.indices) values(i) = values(i) + v.values(i)
      this
    }
  }

  def subtract(v: VectorN): VectorN = {
    if (values.length != v.values.length) throw MismatchedVectorDimensionsException(this, v)
    else {
      for (i <- values.indices) values(i) = values(i) - v.values(i)
      this
    }
  }

  override def copy():VectorN = new VectorN({
    val cp:VectorValues = new VectorValues(values.length)
    for (i <- values.indices) cp(i) = values(i)
    cp
  })

  // copy operators
  def *(scalar:Double):VectorN = copy().scale(scalar)
  def /(divisor:Double):VectorN = copy().divide(divisor).asInstanceOf[VectorN]

  def +(v:VectorN):VectorN = copy().add(v)
  def -(v:VectorN):VectorN = copy().subtract(v)

  def indexedExhaustiveToString(sb:StringBuilder = new StringBuilder(), numberFormatter:Double => String = d => d.toString):StringBuilder = {
    dynamicCustomToString(
      (v:Vector) => s"《${exalt(this.dimension)}↗〉",
      (i:Int) => abase(i) + " ",
      (v:Vector) => "〉",
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