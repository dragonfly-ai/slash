package ai.dragonfly.math.vector

/**
 * Created by clifton on 1/9/17.
 */

class VectorN(val values: Array[Double]) extends BaseVectorN {

  def this(vals: Double*) = this(vals.toArray[Double])

  override def clone(): VectorN = {
    val clonedValues = new Array[Double](values.length)
    for (i <- 0 until values.length) clonedValues(i) = values(i)
    new VectorN(clonedValues)
  }

  override def copy = clone()

  override def hashCode(): Int = {
    var hash = 0
    for(d: Double <- values) hash += d.hashCode()
    hash
  }

}

object VectorN extends UtilVectorN