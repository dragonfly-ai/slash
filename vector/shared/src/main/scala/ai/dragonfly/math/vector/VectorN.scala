package ai.dragonfly.math.vector

import ai.dragonfly.math.examples.Demonstrable

import ai.dragonfly.math.*

/**
 * Created by clifton on 1/9/17.
 */

object VectorN extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    import Console.{GREEN, RED, RESET, YELLOW, UNDERLINED, RED_B}


    val `v⃑₀` = Vector(0.5, 0.0, 1.0, 0.75)
    sb.append("val v⃑₀ = Vector(0.5, 0.0, 1.0, 0.75)")
    .append("\n\tv⃑₀:").append(`v⃑₀`)
    sb.append("\nv⃑₀.scale(3):\n\t").append(`v⃑₀`.scale(3)).append(" /* in place operation */")
    val `v⃑₁` = Vector(5, 6, 7, 8)
    sb.append("\nv⃑₁ = ").append(`v⃑₁`)
    sb.append("\nv⃑₁.add(`v⃑₁`) = ").append(`v⃑₁`.add(`v⃑₁`)).append(" /* in place operation */")
    val `v⃑₂` = Vector(0.25, 0.25, 0.25, 0.25)
    sb.append("\nv⃑₂ = ").append(`v⃑₂`)
    sb.append("\nv⃑₂.dot(v⃑₀) = ").append(`v⃑₂`.dot(`v⃑₀`))
    sb.append("\nv⃑₂ = ").append(`v⃑₂`)
    sb.append("\nv⃑₂.subtract(v⃑₀) = ").append(`v⃑₂`.subtract(`v⃑₀`)).append(" /* in place operation */")
    for (i <- 0 until 10) {
      val v⃑ₜ = Vector.random(4)
      sb.append("\nval v⃑ₜ = Vector.random(4) = ").append(v⃑ₜ)
        .append("\n\t∥").append(v⃑ₜ).append("∥ = ").append(v⃑ₜ.magnitude())
        .append("\n\tv⃑ₜ.normalize() = ").append(v⃑ₜ.normalize()).append(" /* in place operation */")
        .append("\n\t∥").append(v⃑ₜ).append("∥ = ").append(v⃑ₜ.magnitude())
        .append("\n\t").append(v⃑ₜ).append(" * 2.0 = ").append(v⃑ₜ * 2).append(" /* Copy operation */")
        .append("\n\tv⃑ₜ remains unnaffected: ").append(v⃑ₜ)
    }
    sb.append("\n\nVector.random(2).subtract(Vector.random(3)) /* Generates Exception */\n\t")
    try {
      Vector.random(2).subtract(Vector.random(3))
    } catch {
      case e: Throwable => sb.append(e)
    }

    sb.append("\n\nVector.fill(9, 0)").append(Vector.fill(9, 0))

    val r⃑:VectorN = Vector.random(42, 777.777).asInstanceOf[VectorN]
    sb.append("\n\nval r⃑:VectorN = Vector.random(42, 777).asInstanceOf[VectorN]")
      .append("\n\tr⃑.toString: ")
      .append("\n\t\t").append(r⃑)
      .append("\n\tr⃑.exhaustiveToString(): ")
      .append("\n\t\t").append(r⃑.exhaustiveToString())
      .append("\n\tr⃑.exhaustiveToString(numberFormatter = (d:Double) => \"%7.3f\").format(d)): ")
      .append("\n\t\t").append(r⃑.exhaustiveToString(numberFormatter = (d:Double) => "%7.3f".format(d)))
      .append("\n\tr⃑.indexedExhaustiveToString(): ")
      .append("\n\t\t").append(r⃑.indexedExhaustiveToString(numberFormatter = (d:Double) => "%7.3f".format(d)))
      .append("\n\tr⃑.indexedExhaustiveToString(numberFormatter = (d:Double) => \"%7.3f\".format(d)): ")
      .append("\n\t\t").append(r⃑.indexedExhaustiveToString(numberFormatter = (d:Double) => "%7.3f".format(d)))


    sb.append("\n\nVector.midpoint(new VectorN(1.0, 2.0, 3.0, 4.0, 5.0), new VectorN(5.0, 4.0, 3.0, 2.0, 1.0))\n\t")
      .append(Vector.midpoint(new VectorN(1.0, 2.0, 3.0, 4.0, 5.0), new VectorN(5.0, 4.0, 3.0, 2.0, 1.0)))
      .append("\n")

  }

  override def name: String = "VectorN"
}

case class VectorN(values:VectorValues) extends Vector {

  def this(vals:Double*) = this(VectorValues(vals:_*))

  //@JSExport("apply")
  def apply(i: Int): Double = values(i)

  override def copy():VectorN = new VectorN({
    val cp:VectorValues = new VectorValues(values.length)
    for (i <- values.indices) cp(i) = values(i)
    cp
  })

  override def dimension: Int = values.length

  override def component(i: Int): Double = try {
    values(i)
  } catch {
    case aioobe:ArrayIndexOutOfBoundsException => throw ExtraDimensionalAccessException(this, i)
  }

  override def magnitudeSquared(): Double = { var mag2 = 0.0; for (v:Double <- values) mag2 = mag2 + (v*v); mag2 }

  override def distanceSquaredTo(v⃑: Vector): Double = {
    if (values.length != v⃑.values.length) throw MismatchedVectorDimensionsException(this, v⃑)
    else {
      var distance = 0.0
      for ( i <- 0 until v⃑.dimension) {
        val delta = values(i) - v⃑.values(i)
        distance = distance + delta * delta
      }
      distance
    }

  }

  override def dot(v⃑: Vector): Double = {
    if (values.length != v⃑.values.length) throw MismatchedVectorDimensionsException(this, v⃑)
    else {
      var accumulator = 0.0
      for (i <- values.indices) {
        accumulator = accumulator + v⃑.values(i) * values(i)
      }
      accumulator
    }
  }


  override def scale(scalar: Double): VectorN = {
    for (i <- values.indices) values(i) = values(i) * scalar
    this
  }


  override def round(): VectorN = {
    for (i <- values.indices) values(i) = Math.round(values(i)).toDouble
    this
  }

  override def add(v⃑: Vector): VectorN = {
    if (values.length != v⃑.values.length) throw MismatchedVectorDimensionsException(this, v⃑)
    else {
      for (i <- values.indices) values(i) = values(i) + v⃑.values(i)
      this
    }
  }

  override def subtract(v⃑: Vector): VectorN = {
    if (values.length != v⃑.values.length) throw MismatchedVectorDimensionsException(this, v⃑)
    else {
      for (i <- values.indices) values(i) = values(i) - v⃑.values(i)
      this
    }
  }

  def indexedExhaustiveToString(sb:StringBuilder = new StringBuilder(), numberFormatter:Double => String = d => d.toString):StringBuilder = {
    dynamicCustomToString(
      (v⃑:Vector) => s"《${exalt(this.dimension)}↗〉",
      (i:Int) => abase(i) + " ",
      (v⃑:Vector) => "〉",
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