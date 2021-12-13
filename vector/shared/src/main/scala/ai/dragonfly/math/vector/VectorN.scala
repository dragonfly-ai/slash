package ai.dragonfly.math.vector

import ai.dragonfly.math.util.Demonstrable

/**
 * Created by clifton on 1/9/17.
 */

object VectorN extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val v0 = Vector(0.5, 0.0, 1.0, 0.75)
    sb.append("v0 = " + v0)
    sb.append("v0.scale(3) = " + v0.scale(3))
    val v1 = Vector(5, 6, 7, 8)
    sb.append("v1 = " + v1)
    sb.append("v1.add(v1) = " + v1.add(v1))
    val v2 = Vector(0.25, 0.25, 0.25, 0.25)
    sb.append("v2 = " + v2)
    sb.append("v2.dot(v0) = " + v2.dot(v0))
    sb.append("v2 = " + v2)
    sb.append("v2.subtract(v0) = " + v2.subtract(v0))
    for (i <- 0 until 10) {
      val vT = Vector.random(4)
      sb.append("Vector.random(4) => " + vT + " magnitude = " + vT.magnitude() +
        "\n\tnormalized to: " + vT.normalize() +
        "\n\tmagnitude: " + vT.magnitude() +
        "\n\tcopy: " + vT.copy() + " copy.scale(2): " + vT.copy().scale(2)
      )
      sb.append(vT)
    }
    try {
      Vector.random(2).subtract(Vector.random(3))
    } catch {
      case e: Throwable => sb.append(e)
    }

    sb.append(Vector.fill(9, 0))

    sb.append("Vector.random(40, Integer.MAX_VALUE) => " + Vector.random(40, Integer.MAX_VALUE))

    sb.append("midpoint: " + Vector.midpoint(new VectorN(1.0, 2.0, 3.0, 4.0, 5.0), new VectorN(5.0, 4.0, 3.0, 2.0, 1.0)))
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

  override def distanceSquaredTo(v0: Vector): Double = {
    if (values.length != v0.values.length) throw MismatchedVectorDimensionsException(this, v0)
    else {
      var distance = 0.0
      for ( i <- 0 until v0.dimension ) {
        val delta = values(i) - v0.values(i)
        distance = distance + delta * delta
      }
      distance
    }

  }

  override def dot(v0: Vector): Double = {
    if (values.length != v0.values.length) throw MismatchedVectorDimensionsException(this, v0)
    else {
      var accumulator = 0.0
      for (i <- values.indices) {
        accumulator = accumulator + v0.values(i) * values(i)
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

  override def add(v0: Vector): VectorN = {
    if (values.length != v0.values.length) throw MismatchedVectorDimensionsException(this, v0)
    else {
      for (i <- values.indices) values(i) = values(i) + v0.values(i)
      this
    }
  }

  override def subtract(v0: Vector): VectorN = {
    if (values.length != v0.values.length) throw MismatchedVectorDimensionsException(this, v0)
    else {
      for (i <- values.indices) values(i) = values(i) - v0.values(i)
      this
    }
  }

  override def toString:String = {
    val sb = new StringBuilder(Math.min(dimension * 10, 160))
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