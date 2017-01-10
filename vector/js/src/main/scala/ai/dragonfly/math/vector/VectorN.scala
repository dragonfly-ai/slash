package ai.dragonfly.math.vector

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

/**
 * Created by clifton on 1/9/17.
 */
@JSExport class VectorN(override val values: Array[Double]) extends VectorNCapabilities {

  @JSExport def this(vals: Double*) = this(vals.toArray[Double])

  @JSExport def this(values: js.Array[Double]) = this(Util.toScalaArray(values))


  @JSExport override def clone(): VectorN = {
    val clonedValues = new Array[Double](values.length)
    for (i <- 0 until values.length) clonedValues(i) = values(i)
    new VectorN(clonedValues)
  }

  @JSExport def center(vectors: js.Array[VectorNCapabilities]): Array[VectorNCapabilities] = center(
    Util.toScalaArray(vectors)
  )

}

@JSExport("UtilVectorN")
object VectorN extends UtilVectorN {
  @JSExport def average(vectors: js.Array[VectorN]): VectorNCapabilities = average(
    Util.toScalaArray(vectors)
  )

}