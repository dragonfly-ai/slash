package ai.dragonfly.math.vector

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/**
 * Created by clifton on 1/9/17.
 */
@JSExportTopLevel("VectorN") class VectorN(override val values: Array[Double]) extends BaseVectorN {

  @JSExportTopLevel("VectorN.fromValues") def this(vals: Double*) = this(vals.toArray[Double])

  @JSExportTopLevel("VectorN.fromArrayValues") def this(values: js.Array[Double]) = this(Util.toScalaArray(values))


  @JSExport override def clone(): VectorN = {
    val clonedValues = new Array[Double](values.length)
    for (i <- 0 until values.length) clonedValues(i) = values(i)
    new VectorN(clonedValues)
  }

  @JSExport override def copy = clone()

  @JSExport def center(vectors: js.Array[Vector]): Array[Vector] = center(
    Util.toScalaArray(vectors)
  )

}

@JSExportTopLevel("UtilVectorN")
object VectorN extends UtilVectorN {
  @JSExport def average(vectors: js.Array[VectorN]): Vector = average(
    Util.toScalaArray(vectors)
  )

}